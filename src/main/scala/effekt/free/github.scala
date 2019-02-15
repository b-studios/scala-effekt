package effekt
package free

import cats.{ Applicative, Functor, Monoid, Monad }
import cats.implicits._
import scala.concurrent._
import scala.concurrent.duration._
import play.api.libs.json._

import effekt.free.http.Http
object github extends App {

  implicit def lift[R](idiom: Idiom[R]): Eff[R] = embed(idiom)

  // The effect operations
  sealed trait Github[A] extends Op[A]
  case class GetComment(owner: Owner, repo: Repo, id: Int) extends Github[Comment]
  case class GetComments(owner: Owner, repo: Repo, issue: Issue) extends Github[List[Comment]]
  case class GetUser(login: UserLogin) extends Github[User]
  case class ListIssues(owner: Owner, repo: Repo) extends Github[List[Issue]]

  // Boilerplate
  object Github {
    def getComment(owner: Owner, repo: Repo, id: Int) = send(GetComment(owner, repo, id))
    def getComments(owner: Owner, repo: Repo, issue: Issue) = send(GetComments(owner, repo, issue))
    def getUser(login: UserLogin) = send(GetUser(login))
    def getUser(login: String) = send(GetUser(UserLogin(login)))
    def listIssues(owner: Owner, repo: Repo) = send(ListIssues(owner, repo))
  }

  // Data Classes
  case class Issue(value: Int)
  case class Url(value: String)
  case class Owner(value: String)
  case class UserLogin(value: String)
  case class Repo(value: String)
  case class Comment(url: Url, body: Body, user: UserLogin)
  case class Body(value: String) { override def toString = "<body not shown>" }
  case class User(login: String, name: String)


  // Example Program
  // ===============
  def allUsers(owner: Owner, repo: Repo): Eff[List[(Issue,List[(Comment,User)])]] = for {

    issues <- Github.listIssues(owner,repo)

    _ = println("got issues " + issues)

    issueComments <- issues.traverse(issue => Github.getComments(owner,repo,issue).map((issue,_)))

    users <-
      issueComments.traverse { case (issue,comments) =>
        comments.traverse(comment =>
          Github.getUser(comment.user).map((comment,_))).map((issue,_))
      }
  } yield users


  val threeUsers: Idiom[String] =
    Applicative[Idiom].map3(
      Github.getUser("b-studios"),
      Github.getUser("phischu"),
      Github.getUser("klauso")) {
      (bstudios, phischu, klauso) =>
        s"Usernames are ${bstudios.name}, ${phischu.name}, and ${klauso.name}"
    }


//  log { RequestedLogins { threeUsers } }

//  log { http.blocking { githubRemote2 { threeUsers } } }

//  log {
//    import scala.concurrent.ExecutionContext.Implicits.global
//    http.nonblocking(10 seconds) { githubRemote { threeUsers } }
//  }

//  log {
//    import scala.concurrent.ExecutionContext.Implicits.global
//    http.blocking {
//      remote {
//        batched {
//          allUsers(Owner("koka-lang"), Repo("libhandler")) map { _ mkString "\n" }
//        }
//      }
//    }
//  }


  // Github Remote Handler
  // =====================

  object Remote extends Functorial[Idiom] {
    def onPure[R] = Idiom.pure
    def onEffect[X, R] = {
      case GetComment(owner, repo, id) =>
        fetch(s"/repos/${owner.value}/${repo.value}/issues/comments/$id", parseComment)
      case GetComments(owner, repo, issue) =>
        fetch(s"/repos/${owner.value}/${repo.value}/issues/${issue.value}/comments", parseComments)
      case GetUser(login) =>
        fetch(s"/users/${login.value}", parseUser)
      case ListIssues(owner, repo) =>
        fetch(s"/repos/${owner.value}/${repo.value}/issues", parseIssues)
    }

    def fetch[X, R](endpoint: String, parse: Parser[X]): Idiom[X => R] => Idiom[R] = resume =>
      Http.get("https://api.github.com" + endpoint).map(Json.parse).map(parse) ap resume
  }
  def remote[R](prog: Eff[R]): Eff[R] =
    Remote.dynamic(prog)(new Sequencer {
      def apply[X] = x => resume => embed { x } flatMap resume
    })


  // Batched Handlers
  // ================


  // This is a handler that collects the statically known set of requested
  // user logins within an idiomatic computation.
  //
  // ## Example
  // RequestedLogins { Github.getUser(UserLogin("foo")) }
  // > Set(UserLogin("foo"))
  object RequestedLogins extends Monoidal[Set[UserLogin]] {
    def onEffect[X, R] = {
      case GetUser(login) => _ + login
      // this is necessary, otherwise all other effects will
      // be executed when running the program for analysis.
      // TODO use other effects (than github) in the same
      //      program and then change this to `other: Github[_]`
      case other => identity
    }
  }

  // This is a handler that handles `getUser` requests by looking up in a given db
  // and forwards to an outer handler otherwise. Also all other effect operations
  // are forwarded.
  //
  // ## Example
  // prefetched(Map(UserLogin("foo") -> User("foo", "Peter Foo"))) {
  //   Github.getUser(UserLogin("foo"))
  // }
  // > User("foo", "Peter Foo")
  type DB = Map[UserLogin, User]
  class Prefetched(db: DB) extends IdiomaticId {
    def onEffect[X, R] = {
      // TODO forward if not in DB
      //   db.get(login).map(pure).getOrElse(Github.getUser(login))
      // we need to change the signature of `onEffect` for that.
      case GetUser(login) => resume => resume(db(login))
    }
  }
  def prefetched[R](db: DB) = new Prefetched(db)

  // A handler for idiomatic programs that forwards all effect operations
  // to an outer handler but optimizes the requests before.
  // The resulting program is monadic.
  def optimize[R](prog: Idiom[R]): Eff[R] =
    for {
      // (1) statically analyse the *set* of requested logins
      logins <- RequestedLogins { prog } map { _.toList }
      _ = println("prefetching user logins: " + logins)
      // (2) now fetch the necessary users. This is again an idiomatic prog.
      users <- logins.traverse { Github.getUser }
      // (3) build up the db / cache
      db = (logins zip users).toMap
      // (4) use the db to handle the `getUser` requests and forward otherwise
      res <- prefetched(db) { prog }
    } yield res

  object ReifyGithub extends Applicable[Idiom] {
    def interpret[X] = { case g: Github[X] => send(g) }
  }

  // This is a handler that uses `reify` to obtain the idiomatic
  // program which it then runs optimized.
  def batched[R](prog: Eff[R]): Eff[R] =
    ReifyGithub.dynamic(prog)(new Sequencer {
      def apply[R] = prog => resume => optimize(prog) flatMap resume
    })


  // JSON parsers
  // ============

  type Parser[T] = JsValue => T
  private def parseComments: Parser[List[Comment]] = json => {
    val objs = json.validate[List[JsValue]].get
    objs.map { obj =>
      (for {
        url <- (obj \ "url").validate[String]
        body <- (obj \ "body").validate[String]
        login <- (obj \ "user" \ "login").validate[String]
      } yield Comment(Url(url),Body(body),UserLogin(login))).get
    }
  }

  private def parseComment: Parser[Comment] = obj => {
    (for {
      url <- (obj \ "url").validate[String]
      body <- (obj \ "body").validate[String]
      login <- (obj \ "user" \ "login").validate[String]
    } yield Comment(Url(url),Body(body),UserLogin(login))).get
  }

  private def parseUser: Parser[User] = json => {
    (for {
      login <- (json \ "login").validate[String]
      name <- (json \ "name").validate[String] orElse (json \ "login").validate[String]
    } yield User(login,name)).asOpt.get
  }

  private def parseIssues: Parser[List[Issue]] = json => {
    val objs = json.validate[List[JsValue]].get
    objs.map(obj => (obj \ "number").validate[Int].map(Issue(_)).asOpt).flatten
  }
}

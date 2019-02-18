package effekt
package free

import cats.{ Applicative, Functor, Monad, Monoid }
import cats.implicits._

import scala.concurrent._
import scala.concurrent.duration._
import play.api.libs.json._
import effekt.free.http.Http

import scala.reflect.ClassTag

// This example is inspired by Markus Hauck's talk:
//     "Free Monads and Free Applicatives", flatMap(Oslo) 2016.
//     https://github.com/markus1189/flatmap-oslo-2016
object github {

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

  object run extends App {

//  log {
//    import scala.concurrent.ExecutionContext.Implicits.global
//    val r = new BatchedStatic apply { threeUsers }
//    println(r)
//    http.nonblocking(10 seconds) { github.remote { runRequests(10 seconds) { r } }}
//  }


//  log { RequestedLogins { threeUsers } }

//  log { http.blocking { githubRemote2 { threeUsers } } }

//  log {
//    import scala.concurrent.ExecutionContext.Implicits.global
//    http.nonblocking(10 seconds) { githubRemote { threeUsers } }
//  }

  log {
    import scala.concurrent.ExecutionContext.Implicits.global
    http.nonblocking(30 seconds) {
      github.remote {
        github.optimized {
          allUsers(Owner("koka-lang"), Repo("madoko")) map { _ mkString "\n" }
        }
      }
    }
  }

  }

  // Github Remote Handler
  // =====================

  object Remote extends IdiomaticId {
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

    def fetch[X, R](endpoint: String, parse: Parser[X]): Idiom[X => R] => Idiom[R] = resume => {
      println("Requesting github endpoint: " + endpoint)
      Http.get("https://api.github.com" + endpoint).map(Json.parse).map(parse) ap resume
    }
  }
  def remote[R](prog: Eff[R]): Eff[R] =
    Remote.dynamic(prog)(new Sequencer {
      def apply[X] = x => resume => resume(x)
    })


  // Batched Handlers
  // ================
  type DB = Map[UserLogin, User]
  type Id[X] = X

  def requestedLogins(prog: Idiom[_]): Set[UserLogin] =
    prog.fold(Set.empty) { case GetUser(login) => _ + login }

  def optimized[R] = http.Prefetched[UserLogin, User, GetUser](_.login, GetUser.apply).optimized[R]


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

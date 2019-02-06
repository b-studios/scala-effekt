package effekt
package examples

import scala.concurrent._
import scala.concurrent.duration._
import play.api.libs.json._

import cats.Applicative
import cats.implicits._

import effekt.catsinterop._
import effekt.handler._

trait GithubEffect {

  // The effect signature
  trait Github {
    def getComment(owner: Owner, repo: Repo, id: Int): I[Comment]
    def getComments(owner: Owner, repo: Repo, issue: Issue): I[List[Comment]]
    def getUser(login: UserLogin): I[User]
    def listIssues(owner: Owner, repo: Repo): I[List[Issue]]
  }
  def Github: Github using Github = implicit gh => gh

  // Data Classes
  case class Issue(value: Int)
  case class Url(value: String)
  case class Owner(value: String)
  case class UserLogin(value: String)
  case class Repo(value: String)
  case class Comment(url: Url, body: Body, user: UserLogin)
  case class Body(value: String) { override def toString = "<body not shown>" }
  case class User(login: String, name: String)
}

// The example is inspired by Markus Hauk's talk:
//     "Free Monads and Free Applicatives", flatMap(Oslo) 2016.
//     https://github.com/markus1189/flatmap-oslo-2016
trait GithubExamples
    extends GithubEffect
    with GithubStubHandler
    with GithubRemoteHandler
    with GithubBatchedHandler {

  // A user program that is partially idiomatic but overall monadic.
  def allUsers(owner: Owner, repo: Repo): C[List[(Issue,List[(Comment,User)])]] using Github = for {

    issues <- Github.listIssues(owner,repo)

    issueComments <- issues.traverse(issue => Github.getComments(owner,repo,issue).map((issue,_)))

    users <-
      issueComments.traverse { case (issue,comments) =>
        comments.traverse(comment =>
          Github.getUser(comment.user).map((comment,_))).map((issue,_))
      }
  } yield users

  // Running the example with the static stub handler
  println { run {
    allUsers(epfl, dotty)(GithubStub) map {
      _ mkString "\n"
    }
  }}

  // Running the example with the static stub handler
  // AND the batch optimizing handler
  println("-----------")
  println { run {
    batched { implicit _ =>
      allUsers(epfl, dotty) map { _ mkString "\n" }
    } (GithubStub)
  }}

  // Running the example with a simple, blocking remote
  // handler AND the batch optimizing handler.
  // (commented out to avoid running into rate-limiting issues)
//  println("-----------")
//  println { run {
//    batched { implicit _ =>
//      allUsers(Owner("koka-lang"), Repo("libhandler")) map { _ mkString "\n" }
//    } (GithubRemote)
//  }}

  // Running the example with a remote handler that uses
  // the future applicative instance AND the batch optimizing
  // handler.
  import scala.concurrent.ExecutionContext.Implicits.global
  println("-----------")
  println { run {
    githubRemoteFuture(30 seconds) { implicit _ =>
      batched { implicit _ =>
        allUsers(Owner("koka-lang"), Repo("libhandler")) map { _ mkString "\n" }
      }
    }
  }}
}

trait GithubStubHandler extends GithubEffect {

  // just some static dummy data
  val epfl = Owner("epfl")
  val dotty = Repo("dotty")
  val bstudios = UserLogin("b-studios")
  val phischu = UserLogin("phischu")

  object GithubStub extends Github {
    val (i4337, i4339, i5202) = (Issue(4337), Issue(4339), Issue(5202))

    val issues: Map[(Owner, Repo), List[Issue]] = Map(
      (epfl, dotty) -> List(i4337, i4339, i5202)
    )

    val comments: Map[(Owner, Repo, Issue), List[Comment]] = Map(
      (epfl, dotty, i4337) -> List(
        Comment(Url("/foo"), Body("this is great"), bstudios),
        Comment(Url("/bar"), Body("I think so too"), phischu)),
      (epfl, dotty, i4339) -> List(
        Comment(Url("/baz"), Body("when is this going to be fixed?"), bstudios)),
      (epfl, dotty, i5202) -> List
        (Comment(Url("/bam"), Body("yes!"), phischu))
    )

    def getComment(owner: Owner, repo: Repo, id: Int) =
      pure(Comment(Url("..."), Body("..."), UserLogin("..."))) // not yet implemented

    def getComments(owner: Owner, repo: Repo, issue: Issue) = {
      val key = (owner, repo, issue)
      println("getComments on " + key)
      pure(comments(key))
    }

    def getUser(login: UserLogin): I[User] = {
      println("getUser on " + login)
      login match {
        case UserLogin("b-studios") => pure(User("b-studios", "Jonathan"))
        case UserLogin("phischu") => pure(User("phischu", "Philipp"))
      }
    }

    def listIssues(owner: Owner, repo: Repo) = {
      println("listIssues on " + (owner, repo))
      pure(issues((owner, repo)))
    }
  }
}

trait GithubRemoteHandler extends GithubEffect {

  trait GithubApi extends Github {
    def getComment(owner: Owner, repo: Repo, id: Int) =
      fetch(s"/repos/${owner.value}/${repo.value}/issues/comments/$id", parseComment)

    def getComments(owner: Owner, repo: Repo, issue: Issue) =
      fetch(s"/repos/${owner.value}/${repo.value}/issues/${issue.value}/comments", parseComments)

    def getUser(login: UserLogin) =
      fetch(s"/users/${login.value}", parseUser)

    def listIssues(owner: Owner, repo: Repo) =
      fetch(s"/repos/${owner.value}/${repo.value}/issues", parseIssues)

    def fetch[T](uri: String, parse: Parser[T]): I[T]

    // this is a blocking call
    protected def fetch(endpoint: String) =
      Json.parse(requests.get("https://api.github.com" + endpoint).text)
  }

  // A very naive, blocking implementation
  object GithubRemote extends GithubApi {
    def fetch[T](uri: String, parse: Parser[T]): I[T] =
      pure(parse(fetch(uri)))
  }

  // has to be used as the very last effect (all other effects have to be handled before)
  class GithubRemoteFuture[R](implicit ec: ExecutionContext) extends GithubApi with Functorial[Future] {
    def unit[R] = Future.apply
    def fetch[T](uri: String, parse: Parser[T]): I[T] = use { k =>
      Applicative[Future].ap(k) { Future { fetch(uri) }.map(parse) }
    }
  }

  def githubRemoteFuture[R](timeout: Duration)(prog: C[R] using Github): C[R] using ExecutionContext =
    new GithubRemoteFuture().dynamic[R](prog) { prog: Future[ω] => resume: (ω => C[R]) =>
      Await.result(prog.map(resume), timeout)
    }


  // JSON parsers
  type Parser[T] = JsValue => T
  private val parseComments: Parser[List[Comment]] = json => {
    val objs = json.validate[List[JsValue]].get
    objs.map { obj =>
      (for {
        url <- (obj \ "url").validate[String]
        body <- (obj \ "body").validate[String]
        login <- (obj \ "user" \ "login").validate[String]
      } yield Comment(Url(url),Body(body),UserLogin(login))).get
    }
  }

  private val parseComment: Parser[Comment] = obj => {
    (for {
      url <- (obj \ "url").validate[String]
      body <- (obj \ "body").validate[String]
      login <- (obj \ "user" \ "login").validate[String]
    } yield Comment(Url(url),Body(body),UserLogin(login))).get
  }

  private val parseUser: Parser[User] = json => {
    (for {
      login <- (json \ "login").validate[String]
      name <- (json \ "name").validate[String] orElse (json \ "login").validate[String]
    } yield User(login,name)).asOpt.get
  }

  private val parseIssues: Parser[List[Issue]] = json => {
    println(json)
    val objs = json.validate[List[JsValue]].get
    objs.map(obj => (obj \ "number").validate[Int].map(Issue(_)).asOpt).flatten
  }
}

trait GithubBatchedHandler extends GithubEffect {

  class RequestedLogins extends Github with Analyze[Set[UserLogin]] with Monoidal[Set[UserLogin]] {
    def getUser(login: UserLogin): I[User] = collect { Set(login) }
    def getComment(owner: Owner, repo: Repo, id: Int) = default
    def getComments(owner: Owner, repo: Repo, issue: Issue) = default
    def listIssues(owner: Owner, repo: Repo) = default
  }
  def requestedLogins = new RequestedLogins

  type DB = Map[UserLogin, User]
  class Prefetched(db: DB)(implicit outer: Github) extends Github with Id {
    def getUser(login: UserLogin): I[User] =
      db.get(login).map(pure).getOrElse(Github.getUser(login))

    def getComment(owner: Owner, repo: Repo, id: Int) =
      Github.getComment(owner, repo, id)
    def getComments(owner: Owner, repo: Repo, issue: Issue) =
      Github.getComments(owner, repo, issue)
    def listIssues(owner: Owner, repo: Repo) =
      Github.listIssues(owner, repo)
  }
  def prefetched[R](db: Map[UserLogin, User]): Prefetched using Github =
    new Prefetched(db)

  // we can also run the handler on monadic computations!
  def prefetchedM[R](db: Map[UserLogin, User])(prog: C[R] using Github): C[R] using Github =
    prefetched(db) handleMonadic prog

  class Reify extends Github with Idiomatic {
    type G[X] = I[X] using Github
    def unit[R] = r => pure(r)
    def map[A, B] = f => ma => ma map f

    def getUser(login: UserLogin): I[User] = use { Github.getUser(login) ap _ }
    def getComment(owner: Owner, repo: Repo, id: Int) = use { Github.getComment(owner, repo, id) ap _ }
    def getComments(owner: Owner, repo: Repo, issue: Issue) = use { Github.getComments(owner, repo, issue) ap _ }
    def listIssues(owner: Owner, repo: Repo) = use { Github.listIssues(owner, repo) ap _ }
  }
  def reify = new Reify

  def batched[R](prog: C[R] using Github): C[R] using Github =
    reify.dynamic(prog) {
      prog => resume => for {
        logins <- requestedLogins { prog } map { _.toList }
        _ = println("prefetching user logins: " + logins)
        users <- logins.traverse { Github.getUser } // here we could actually batch.
        db = (logins zip users).toMap
        res <- prefetched(db) handle { prog } flatMap resume
      } yield res
    }
}
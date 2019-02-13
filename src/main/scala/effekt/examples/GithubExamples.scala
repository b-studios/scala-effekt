package effekt
package examples

import scala.concurrent._
import scala.concurrent.duration._
import play.api.libs.json._

import cats.Applicative
import cats.implicits._

import effekt.catsinterop._
import effekt.handler._

// This example is inspired by Markus Hauck's talk:
//     "Free Monads and Free Applicatives", flatMap(Oslo) 2016.
//     https://github.com/markus1189/flatmap-oslo-2016
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
    githubRemoteParallel(30 seconds) { implicit _ =>
      batched { implicit _ =>
        allUsers(Owner("koka-lang"), Repo("madoko")) map { _ mkString "\n" }
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

  // A blocking handler that sends HTTP requests to the Github API.
  object GithubRemote extends GithubApi {
    def fetch[T](uri: String, parse: Parser[T]): I[T] =
      pure(parse(fetch(uri)))
  }

  case class Parallel[A](requests: List[String], future: Future[A])

  implicit def parallelApplicative(implicit ec: ExecutionContext): Applicative[Parallel] = new Applicative[Parallel] {
    def ap[A, B](ff: Parallel[A => B])(fa: Parallel[A]): Parallel[B] = (ff, fa) match {
      case (Parallel(reqs1, fa), Parallel(reqs2, a)) => Parallel(reqs1 ++ reqs2, fa ap a)
    }
    def pure[A](a: A): Parallel[A] = Parallel(Nil, Future { a })
    override def map[A, B](fa: Parallel[A])(f: A => B): Parallel[B] = fa match {
      case Parallel(reqs, future) => Parallel(reqs, future map f)
    }
  }


  // An *idiomatic* handler that sends HTTP requests to the Github API.
  // The applicative instance of Future is used to send request
  // concurrently.
  class GithubRemoteFuture[R](implicit ec: ExecutionContext) extends GithubApi with Functorial[Future] {
    def unit[R] = Future.apply
    def fetch[T](uri: String, parse: Parser[T]): I[T] = usePure { k =>
      Applicative[Future].ap(k) { Future { fetch(uri) }.map(parse) }
    }
  }

  class GithubRemoteParallel[R](implicit ec: ExecutionContext) extends GithubApi with Applicable[Parallel] {
    def fetch[T](uri: String, parse: Parser[T]): I[T] = lift {  Parallel(List(uri), Future { fetch(uri) }).map(parse) }
  }
  def githubRemoteParallel[R](timeout: Duration)(prog: C[R] using Github): C[R] using ExecutionContext =
    new GithubRemoteParallel().dynamic[R](prog) { case Parallel(reqs, future) => resume: (ω => C[R]) =>
      println("Requesting in parallel: " + reqs)
      resume(Await.result(future, timeout))
    }

  class GithubRemoteFutureApp[R](implicit ec: ExecutionContext) extends GithubApi with Applicable[Future] {
    def fetch[T](uri: String, parse: Parser[T]): I[T] = lift {  Future { fetch(uri) }.map(parse) }
  }

  def githubRemoteFutureIdiomatic[R](prog: I[R] using Github): I[Future[R]] using ExecutionContext =
    new GithubRemoteFutureApp() handle prog

  def githubRemoteFuture[R](timeout: Duration)(prog: C[R] using Github): C[R] using ExecutionContext =
    new GithubRemoteFutureApp().dynamic[R](prog) { prog: Future[ω] => resume: (ω => C[R]) =>
      resume(Await.result(prog, timeout))
    }

  // Common implementation details of the naive blocking handler and
  // the Future based idiomatic handler.
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
    val objs = json.validate[List[JsValue]].get
    objs.map(obj => (obj \ "number").validate[Int].map(Issue(_)).asOpt).flatten
  }
}

trait GithubBatchedHandler extends GithubEffect {

  // This is a handler that collects the statically known set of requested
  // user logins within an idiomatic computation.
  //
  // ## Example
  // requestedLogins { Github.getUser(UserLogin("foo")) }
  // > Set(UserLogin("foo"))
  class RequestedLogins extends Github with Analyze[Set[UserLogin]] with Monoidal[Set[UserLogin]] {
    def getUser(login: UserLogin): I[User] = collect { Set(login) }
    def getComment(owner: Owner, repo: Repo, id: Int) = default
    def getComments(owner: Owner, repo: Repo, issue: Issue) = default
    def listIssues(owner: Owner, repo: Repo) = default
  }
  def requestedLogins = new RequestedLogins

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

  // A handler that rebuilds the idiomatic program with Github still unhandled.
  // Handlers like this can potentially be generated or implemented using reflection.
  class Reify extends Github with Idiomatic {
    type G[X] = I[X] using Github
    def unit[R] = r => pure(r)
    def map[A, B] = f => ma => ma map f

    def getUser(login: UserLogin): I[User] = usePure { Github.getUser(login) ap _ }
    def getComment(owner: Owner, repo: Repo, id: Int) = usePure { Github.getComment(owner, repo, id) ap _ }
    def getComments(owner: Owner, repo: Repo, issue: Issue) = usePure { Github.getComments(owner, repo, issue) ap _ }
    def listIssues(owner: Owner, repo: Repo) = usePure { Github.listIssues(owner, repo) ap _ }
  }
  def reify = new Reify

  // A handler for idiomatic programs that forwards all effect operations
  // to an outer handler but optimizes the requests before.
  // The resulting program is monadic.
  def optimize[R](prog: I[R] using Github): C[R] using Github =
    for {
      // (1) statically analyse the *set* of requested logins
      logins <- requestedLogins { prog } map { _.toList }
      _ = println("prefetching user logins: " + logins)
      // (2) now fetch the necessary users. This is again an idiomatic prog.
      users <- logins.traverse { Github.getUser }
      // (3) build up the db / cache
      db = (logins zip users).toMap
      // (4) use the db to handle the `getUser` requests and forward otherwise
      res <- prefetched(db) handle { prog }
    } yield res

  // This is a handler that uses `reify` to obtain the idiomatic
  // program which it then runs optimized.
  def batched[R](prog: C[R] using Github): C[R] using Github =
    reify.dynamic(prog) { prog => resume =>
      optimize(prog) flatMap resume
    }
}
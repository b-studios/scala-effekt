package effekt
package free

import cats.{ Applicative, Functor, Monoid, Monad }
import cats.implicits._
import scala.concurrent._
import scala.concurrent.duration._
import play.api.libs.json._
import effekt.free.http.Http

object twitter {

  sealed trait Twitter[A] extends Op[A]
  case class GetUser(login: String) extends Twitter[User]
  object Twitter {
    def getUser(login: String) = send(GetUser(login))
  }

  case class User(login: String, name: String)

  object Remote extends Functorial[Idiom] {
    def onPure[R] = Idiom.pure
    def onEffect[X, R] = {
      case GetUser(login) => fetch(s"/users/${login}", parseUser)
    }
    // for now we pretend github is twitter... :)
    def fetch[X, R](endpoint: String, parse: Parser[X]): Idiom[X => R] => Idiom[R] = resume =>
      Http.get("https://api.github.com" + endpoint).map(Json.parse).map(parse) ap resume
  }
  def remote[R](prog: Eff[R]): Eff[R] =
    Remote.dynamic(prog)(new Sequencer {
      def apply[X] = x => resume => embed { x } flatMap resume
    })

  object RequestedLogins extends Monoidal[Set[String]] {
    def onEffect[X, R] = { case GetUser(login) => _ + login }
  }

  type DB = Map[String, User]
  class Prefetched(db: DB) extends IdiomaticId {
    def onEffect[X, R] = {
      // TODO forward if not in DB
      //   db.get(login).map(pure).getOrElse(Github.getUser(login))
      // we need to change the signature of `onEffect` for that.
      case GetUser(login) => resume => resume(db(login))
    }
  }
  def prefetched[R](db: DB) = new Prefetched(db)

  def optimize[R](prog: Idiom[R]): Eff[R] =
    for {
      // (1) statically analyse the *set* of requested logins
      logins <- embed { RequestedLogins { prog } map { _.toList } }
      _ = println("prefetching user logins: " + logins)
      // (2) now fetch the necessary users. This is again an idiomatic prog.
      users <- embed { logins.traverse { Twitter.getUser } }
      // (3) build up the db / cache
      db = (logins zip users).toMap
      // (4) use the db to handle the `getUser` requests and forward otherwise
      res <- embed { prefetched(db) { prog } }
    } yield res


  type Parser[T] = JsValue => T
  private def parseUser: Parser[User] = json => {
    (for {
      login <- (json \ "login").validate[String]
      name <- (json \ "name").validate[String] orElse (json \ "login").validate[String]
    } yield User(login,name)).asOpt.get
  }
}
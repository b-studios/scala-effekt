package effekt
package examples

import cats.{ Applicative, Functor, Monoid, Monad }
import cats.implicits._
import scala.concurrent._
import scala.concurrent.duration._
import play.api.libs.json._
import http.Http

object twitter {

  sealed trait Twitter[A] extends Op[A]
  case class GetUser(login: String) extends Twitter[User]
  object Twitter {
    def getUser(login: String) = send(GetUser(login))
  }

  case class User(login: String, name: String)

  // While we could implement this handler with Idiomatic[Idiom] to be
  // effectful, having built-in support for an effectful domain is important:
  // With G = Id, the sequencing does not change the idiomatic structure, while with
  // G = Idiom, we need to implement the sequencer as `embed { x } flatMap resume`
  // which leads to loss of idiomatic structure.
  object Remote extends IdiomaticId {
    def onEffect[X, R] = {
      case GetUser(login) => fetch(s"/users/${login}", parseUser)
    }
    // for now we pretend github is twitter... :)
    def fetch[X, R](endpoint: String, parse: Parser[X]): Idiom[X => R] => Idiom[R] = resume => {
      println("Requesting twitter endpoint: " + endpoint)
      Http.get("https://api.github.com" + endpoint).map(Json.parse).map(parse) ap resume
    }
  }
  def remote[R](prog: Eff[R]): Eff[R] =
    Remote.dynamic(prog)(new Sequencer {
      def apply[X] = x => resume => resume(x)
    })

  def optimized[R] = http.Prefetched[String, User, GetUser](_.login, GetUser.apply).optimized[R]


  type Parser[T] = JsValue => T
  private def parseUser: Parser[User] = json => {
    (for {
      login <- (json \ "login").validate[String]
      name <- (json \ "name").validate[String] orElse (json \ "login").validate[String]
    } yield User(login,name)).asOpt.get
  }
}
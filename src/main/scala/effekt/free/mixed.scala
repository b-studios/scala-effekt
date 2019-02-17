package effekt
package free

import cats.{ Applicative, Functor, Monoid, Monad }
import cats.implicits._
import scala.concurrent._
import scala.concurrent.duration._
import play.api.libs.json._

import effekt.free.http.Http
import github.Github
import twitter.Twitter

object mixed extends App {

  val users = List("b-studios", "klauso", "b-studios")

  val prog = users.traverse { u =>
    Applicative[Idiom].map2(Github.getUser(u), Twitter.getUser(u)) {
      (gh, tw) => s"Github: ${gh}, Twitter: ${tw}"
    }
  }

  log {
    import scala.concurrent.ExecutionContext.Implicits.global

    http.nonblocking(5 seconds) {
      // prefetching http requests
      http.prefetched {
        twitter.remote {
          github.remote {
            // prefetching twitter users
            twitter.optimized {
              // prefetching github users
              github.optimized {
                embed { prog }
              }
            }
          }
        }
      }
    }
  }

//  log {
//    import scala.concurrent.ExecutionContext.Implicits.global
//    http.nonblocking(5 seconds) {
//      twitter.Remote {
//        github.Remote {
//          prog
//        }
//      }
//    }
//  }
}
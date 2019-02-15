package effekt
package free

import cats.{ Applicative, Functor, Monoid, Monad }
import cats.implicits._
import scala.concurrent._
import scala.concurrent.duration._

object http {

  case class Get(url: String) extends Op[String]
  object Http {
    def get(url: String) = send(Get(url))
  }

  class Blocking[R] extends Monadic[R] {
    def onEffect[X] = {
      case Get(url) => resume => {
        println("Requesting: " + url)
        resume(requests.get(url).text)
      }
    }
  }
  def blocking[R] = new Blocking[R]

  case class Parallel[A](requests: List[String], future: Future[A])

  implicit def parallelApplicative(implicit ec: ExecutionContext): Applicative[Parallel] = new Applicative[Parallel] {
    def pure[A](a: A): Parallel[A] = Parallel(Nil, Future { a })
    def ap[A, B](ff: Parallel[A => B])(fa: Parallel[A]): Parallel[B] = (ff, fa) match {
      case (Parallel(reqs1, fa), Parallel(reqs2, a)) => Parallel(reqs1 ++ reqs2, fa ap a)
    }
    override def map[A, B](fa: Parallel[A])(f: A => B): Parallel[B] = fa match {
      case Parallel(reqs, future) => Parallel(reqs, future map f)
    }
  }


  // An *idiomatic* handler that sends HTTP requests.
  // The applicative instance of Future is used to send request concurrently.
  class Nonblocking(implicit ec: ExecutionContext) extends Functorial[Parallel] {
    def onPure[R] = r => Parallel(Nil, Future { r })
    def onEffect[X, R] = {
      case Get(url) => resume =>
        resume ap Parallel(List(url), Future { requests.get(url).text })
    }
  }
  def nonblocking[R](timeout: Duration)(prog: Eff[R]): Eff[R] using ExecutionContext =
    new Nonblocking().dynamic[R](prog)(new Sequencer {
      def apply[X] = { case Parallel(reqs, future) => resume =>
        println("Requesting in parallel: " + reqs)
        resume(Await.result(future, timeout))
      }
    })
}

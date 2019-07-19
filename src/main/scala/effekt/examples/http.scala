package effekt
package examples

import cats.{ Applicative, Functor, Monad, Monoid }
import cats.implicits._

import scala.concurrent._
import scala.concurrent.duration._
import scala.reflect.ClassTag

object http {

  type Url = String

  case class Get(url: Url) extends Op[String]
  object Http {
    def get(url: Url) = send(Get(url))
  }

  def blocking[R] = handler {
    case Get(url) -> resume =>
      println("Requesting: " + url)
      resume(requests.get(url).text)
  }

  case class Parallel[A](requests: List[Url], future: Future[A])

  implicit def parallelApplicative(implicit ec: ExecutionContext): Applicative[Parallel] = new Applicative[Parallel] {
    def pure[A](a: A): Parallel[A] = Parallel(Nil, Future.successful { a })
    def ap[A, B](ff: Parallel[A => B])(fa: Parallel[A]): Parallel[B] = (ff, fa) match {
      case (Parallel(reqs1, fa), Parallel(reqs2, a)) => Parallel(reqs1 ++ reqs2, fa ap a)
    }
    override def map[A, B](fa: Parallel[A])(f: A => B): Parallel[B] = fa match {
      case Parallel(reqs, future) => Parallel(reqs, future map f)
    }
  }


  // An *idiomatic* handler that sends HTTP requests.
  // The applicative instance of Future is used to send request concurrently.
  class Nonblocking(implicit ec: ExecutionContext) extends Applicable[Parallel] {
    def interpret[X] = { case Get(url) => Parallel(List(url), Future { requests.get(url).text }) }
  }
  def nonblocking[R](timeout: Duration)(prog: Eff[R]) given ExecutionContext: Eff[R] =
    new Nonblocking().dynamic[R](prog)(new Sequencer {
      def apply[X] = { case Parallel(reqs, future) => resume =>
        println("Requesting in parallel: " + reqs)
        resume(Await.result(future, timeout))
      }
    })

  def prefetched[R] = Prefetched[Url, String, Get](_.url, Get.apply).optimized[R]

  case class Prefetched[K, V, O <: Op[V] : ClassTag](
    cacheKey: O => K,
    runQuery: K => O
  ) {
    type DB = Map[K, V]

    def computeKeys(prog: Idiom[_]): Set[K] =
      prog.fold(Set.empty) { case o : O => _ + cacheKey(o) }

    // Idiom[R] => Idiom[DB => R]
    object fromDB extends Applicable[[X] =>> DB => X] {
      def interpret[X] = { case o : O => db => db(cacheKey(o)) }
    }

    // Idiom[R] => Idiom[R]
    class Optimized[R] extends DynamicHandler[R, [X] =>> X] {
      def handle[X] = prog => {
        val keys  = computeKeys { prog }.toList
        val batched: Idiom[DB] =
          keys.traverse(k => send(runQuery(k))) map {
            values => (keys zip values).toMap
          }
        batched ap fromDB(prog)
      }
      def sequence[X] = r => resume => resume(r)
      override def handles = {
        case o: O => true
        case _ => false
      }
    }

    def optimized[R] = new Optimized[R]
  }

}

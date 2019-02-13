package effekt

import cats.{ Applicative, Functor, Monoid, Monad }
import effekt.handler._

package object catsinterop {

  implicit object controlMonad extends Monad[Control] {
    override def flatMap[A, B](fa: C[A])(f: A => C[B]): C[B] = fa flatMap f
    override def pure[A](a: A): C[A] = effekt.pure(a)

    // TODO make tailrecursive
    // @annotation.tailrec
    def tailRecM[A, B](init: A)(fn: A => C[Either[A, B]]): C[B] =
      fn(init) flatMap {
        case Left(a) => tailRecM(a)(fn)
        case Right(b) => pure(b)
      }
  }

  implicit object idiomApplicative extends Applicative[Idiom] {
    def ap[A, B](ff: I[A => B])(fa: I[A]): I[B] = fa ap ff
    def pure[A](a: A): I[A] = effekt.pure(a)
    override def map[A, B](fa: I[A])(f: A => B): I[B] = fa map f
  }


  // Additional handlers that use cats
  trait Functorial[F[_]: Functor] extends Idiomatic {
    type G[X] = F[X]
    def map[A, B] = f => fa => Functor[F].map(fa)(f)
  }

  trait Monoidal[D](implicit val m: Monoid[D]) extends Idiomatic {
    type G[X] = D
    def unit[R] = r => m.empty
    def map[A, B] = f => d => d
  }

  // since Applicative is already taken, we use the made up name "Applicable"
  trait Applicable[F[_]: Applicative] extends Idiomatic {
    type G[X] = F[X]
    def unit[R] = Applicative[F].pure
    def map[A, B] = f => fa => Applicative[F].map(fa)(f)
    def lift[A](fa: F[A]): I[A] = usePure(Applicative[F].ap(_)(fa))
  }

  trait Analyze[D] extends Monoidal[D] {
    def default[R] = use { identity }
    def collect[A](el: D): I[A] = usePure { d => m.combine(el, d) }
  }
}


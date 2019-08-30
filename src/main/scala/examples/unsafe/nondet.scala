package examples.unsafe

import effekt.unsafe._

object nondeterminism extends App {

  trait Exc { def raise(msg: String): Control[Nothing] }
  def Exc given (e: Exc): e.type = e

  trait Amb { def flip(): Control[Boolean] }
  def Amb given (a: Amb): a.type = a

  def drunkFlip given Exc given Amb = for {
    caught <- Amb.flip()
    r <- if (caught) Amb.flip() else Exc.raise("oops")
  } yield if (r) "heads" else "tails"

  def maybe[R](prog: given Exc => Control[R]): Control[Option[R]] = handle {
    prog given new Exc {
      def raise(msg: String) = scope { pure(None) }
    } map { Some(_) }
  }

  def collect[R](prog: given Amb => Control[R]): Control[List[R]] = handle {
    prog given new Amb {
      def flip() = scope {
      for {
        xs <- resume(true)
        ys <- resume(false)
      } yield xs ++ ys }
    } map { List(_) }
  }

  println { run { maybe { collect { drunkFlip } } } }
  println { run { collect { maybe { drunkFlip } } } }

  object asTraits {
    trait Maybe[R] given Scope [Option[R]] extends Exc {
      def raise(msg: String) = scope { pure(None) }
    }
    def maybe[R](prog: given Exc => Control[R]): Control[Option[R]] =
      handle { prog given new Maybe[R] {} map { Some(_) } }

    trait Collect[R] given Scope [List[R]] extends Amb {
      def flip() = scope { for {
        ts <- resume(true)
        fs <- resume(false)
      } yield ts ++ fs }
    }
    def collect[R, E](prog: given Amb => Control[R]): Control[List[R]] =
      handle { prog given new Collect[R] {} map { List(_) } }
  }
}
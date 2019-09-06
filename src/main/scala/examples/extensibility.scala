package examples

import effekt._
import nondet._

object extensibility extends App {
  trait Choose extends Amb {
    def choose[A](first: A, second: A): A / effect
  }

  trait Nondet extends Amb with Exc

  trait Maybe[R, E] extends Exc {
    val scope: Scope[Option[R], E]
    type effect = scope.effect
    def raise(msg: String) = scope.switch { resume => pure(None) }
  }

  trait Collect[R, E] extends Amb {
    val scope: Scope[List[R], E]
    type effect = scope.effect
    def flip() = scope.switch { resume =>
      for {
        xs <- resume(true);
        ys <- resume(false)
      } yield xs ++ ys
    }
  }

  def maybe[R, E](prog: (exc: Exc) => R / (exc.effect & E)): Option[R] / E =
    handle { s => prog(new Maybe[R, E] { val scope: s.type = s  }) map { r => Some(r) } }

  def collect[R, E](prog: (amb: Amb) => R / (amb.effect & E)): List[R] / E =
    handle { s => prog(new Collect[R, E] { val scope: s.type = s }) map { r => List(r) } }

  trait CollectChoose[R, E] extends Collect[R, E] with Choose {
    def choose[A](first: A, second: A): A / effect = for {
      b <- flip()
    } yield if (b) first else second
  }

  trait Backtrack[R, E] extends Amb {
    val scope: Scope[Option[R], E]
    type effect = scope.effect
    def flip() = scope.switch { resume => for {
      attempt <- resume(true)
      res <- if (attempt.isDefined) pure(attempt) else resume(false)
    } yield res }
  }

  def nondet[R, E](prog: (b: Nondet) => R / (b.effect & E)): Option[R] / E = handle { s =>
    val n = new Nondet with Backtrack[R,E] with Maybe[R,E] { val scope: s.type = s }
    prog(n) map { r => Some(r) }
  }

  println { run { nondet { n => drunkFlip(n, n) } } }


}
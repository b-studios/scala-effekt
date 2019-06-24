package effekt
package examples
import utils._

object DottyTest extends App {

  // Effect Signatures
  trait Exc extends Eff {
    def raise(msg: String): Nothing / effect
  }
  def Exc given (cap: Exc): cap.type = cap

  trait Amb extends Eff {
    def flip(): Boolean / effect

    def choose[A, E](c1: A / E, c2: A / E): A / (effect & E) = for {
      b   <- flip()
      res <- if (b) c1 else c2
    } yield res
  }
  def Amb given (cap: Amb): cap.type = cap

  // Effect Usage

  def drunkFlip(amb: Amb, exc: Exc): String / (amb.effect & exc.effect) = for {
    caught <- amb.flip()
    heads <- if (caught) amb.flip() else exc.raise("Too drunk")
  } yield if (heads) "Heads" else "Tails"

  // can be inferred
  def drunkFlip2 given Amb given Exc = for {
    caught <- Amb.flip()
    heads <- Amb.choose(Amb.flip(), Exc.raise("Too drunk"))
  } yield if (heads) "Heads" else "Tails"


  lazy val x = 0

  def flipTwice given Amb  =
    for {
      x <- Amb.flip()
      y <- Amb.flip()
    } yield x || y

  def prog given Exc given Amb =
    if (x <= 0) Amb.flip() else Exc.raise("too big")

  def div(x: Int, y: Int) given Exc =
    if (y == 0) Exc.raise("y is zero") else pure(x / y)


  // Effect Handlers
  class Maybe[R, FX] extends Exc with Handler[R, Option[R]] {
    type effects = FX
    def unit = r => pure(Some(r))
    def raise(msg: String): Nothing / effect = use { pure(None) }
  }

  class Collect[R, FX] extends Amb with Handler[R, List[R]] {
    type effects = FX
    def unit = r => pure(List(r))
    def flip(): Boolean / effect = use {
      for { xs <- resume(true); ys <- resume(false) }
        yield xs ++ ys
    }
  }


  // Handling of Effects

  val res0 = run { new Collect handle { flipTwice } }

  val res1 = run {
    new Collect handle {
      new Maybe[String, Amb.effect] handle {
        drunkFlip2
      }
    }
  }

  val res2 = run {
    new Collect[Option[String], Pure] handle {
      new Maybe handle {
        drunkFlip2
      }
    }
  }

  println(res2)

  val res3 = run {
    new Collect [Option[Boolean], Pure] handle {
      new Maybe handle {
        prog
      }
    }
  }

  println(res3)
}
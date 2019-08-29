package examples

import effekt._

object nondeterminism extends App {

  trait Exc extends Eff { def raise(msg: String): Nothing / effect }
  def Exc given (e: Exc): e.type = e

  trait Amb extends Eff { def flip(): Boolean / effect }
  def Amb given (a: Amb): a.type = a

  def drunkFlip(exc: Exc, amb: Amb): Control[String, exc.effect & amb.effect] = for {
    caught <- amb.flip()
    r <- if (caught) amb.flip() else exc.raise("oops")
  } yield if (r) "heads" else "tails"

  def drunkFlipI given Exc given Amb = for {
    caught <- Amb.flip()
    r <- if (caught) Amb.flip() else Exc.raise("oops")
  } yield if (r) "heads" else "tails"


  trait Handler[R, E] {
    // this is only necessary since we don't use traits with constructor parameters.
    type Effects
    val use: Scope[R, Effects & E]
    type effect = use.effect
  }

  object withhandler {

    trait Maybe[R, E] extends Exc with Handler[Option[R], E]{
      // using some state effect
      val state: State
      type Effects <: state.effect
      private val myField = state.Field[Int](42)
      def raise(msg: String) = use { resume =>
        myField.update(_ + 1) andThen { pure(None) }
      }
    }

    trait First[R, E] extends Amb with Handler[Option[R], E] {
      val mystate: State
      private val myField = mystate.Field[Boolean](true)

      type Effects <: mystate.effect
      def flip() = use { resume => for {
        b <- myField.value
        t <- resume(b)
        r <- if (t.isDefined) pure(t) else resume(false)
      } yield r }
    }

    trait Both[R, E] extends Maybe[R, E] with First[R, E]
    def both[R, E](prog: (e: Exc, a: Amb) => Control[R, e.effect & a.effect & E]): Control[Option[R], E] =
      region { s =>
        handle[Option[R], E & s.effect] { p =>
          val both = new Both[R, E & s.effect] {
            val state: s.type = s
            val mystate: s.type = s
            val use: p.type = p
            type Effects = s.effect & E
          }
          prog(both, both) map { r => Some(r) }
        }
      }
  }

  object withhandlersimple {

    trait Maybe[R, E] extends Exc with Handler[Option[R], E] {
      type Effects = Pure
      def raise(msg: String) = use { resume => pure(None) }
    }
    def maybe[R, E](prog: (e: Exc) => Control[R, e.effect & E]): Control[Option[R], E] =
      handle { p =>
        prog(new Maybe[R, E] { val use: p.type = p }) map { r => Some(r) }
      }

    def maybe2[R, E](prog: (e: Exc) => Control[R, e.effect & E]): Control[Option[R], E] = handle { scope =>
      val exc = new Exc {
        type effect = scope.effect
        def raise(msg: String) = scope switch { resume => pure(None) }
      }
      prog(exc) map { r => Some(r) }
    }

    def maybeState[R, E](prog: (e: Exc) => Control[R, e.effect & E]): Control[Option[Option[R]], E] =
      maybe2 { exc2 =>
        region[Option[R], exc2.effect & E] { s =>
          handle[Option[R], s.effect & exc2.effect & E] { scope =>
            val exc = new Exc {
              type effect = scope.effect

              val field = s.Field(0)

              def raise(msg: String) = scope switch { resume =>
                for {
                  _ <- field.value = 1
                  _ <- exc2.raise(msg)
                } yield None
              }
            }
            prog(exc) map { r => Some(r) }
          }
        }
      }

    trait Collect[R, E] extends Amb with Handler[List[R], E] {
      type Effects = Pure
      def flip() = use { resume => for {
        ts <- resume(true)
        fs <- resume(false)
      } yield ts ++ fs }
    }
    def collect[R, E](prog: (a: Amb) => Control[R, a.effect & E]): Control[List[R], E] = handle { p =>
      prog(new Collect[R, E] { val use: p.type = p }) map { r => List(r) }
    }
  }

}
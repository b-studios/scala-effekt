package examples.safe

import effekt._

object nondeterminism extends App {

  trait Exc extends Eff { def raise(msg: String): Nothing / effect }
  def Exc given (e: Exc): e.type = e

  trait Amb extends Eff { def flip(): Boolean / effect }
  def Amb given (a: Amb): a.type = a

  def drunkFlip given Exc given Amb = for {
    caught <- Amb.flip()
    r <- if (caught) Amb.flip() else Exc.raise("oops")
  } yield if (r) "heads" else "tails"

  object asTraits {
    trait Maybe[R, E] extends Exc {
      val scope: Scope[Option[R], E]
      type effect = scope.effect
      def raise(msg: String) = scope { pure(None) }
    }
    def maybe[R, E](prog: given (e: Exc) => R / (e.effect & E)) =
      handle [Option[R], E] {
        prog given new Maybe[R, E] { val scope: Scope.type = Scope } map { Some(_) }
      }

    trait Collect[R, E] extends Amb {
      val scope: Scope[List[R], E]
      type effect = scope.effect
      def flip() = scope { for {
        ts <- resume(true)
        fs <- resume(false)
      } yield ts ++ fs }
    }
    // XXX removing `given s => ` here makes type inference worse.
    def collect[R, E](prog: given (a: Amb) => R / (a.effect & E)): List[R] / E =
      handle { given s =>
        prog given new Collect[R, E] { val scope: Scope.type = Scope } map { List(_) }
      }
  }

  trait Handler[R, E] {
    // this is only necessary since we don't use traits with constructor parameters.
    type Effects
    val scope: Scope[R, Effects & E]
    type effect = scope.effect
  }

  object withHandlerTrait {

    trait Maybe[R, E] extends Exc with Handler[Option[R], E] {
      type Effects = Pure
      def raise(msg: String) = scope { pure(None) }
    }
    def maybe[R, E](prog: given (e: Exc) => R / (e.effect & E)) = handle [Option[R], E] {
      prog given new Maybe[R, E] { val scope: Scope.type = Scope } map { Some(_) }
    }

    trait Collect[R, E] extends Amb with Handler[List[R], E] {
      type Effects = Pure
      def flip() = scope { for {
        ts <- resume(true)
        fs <- resume(false)
      } yield ts ++ fs }
    }
    // XXX removing `given s => ` here makes type inference worse.
    def collect[R, E](prog: given (a: Amb) => R / (a.effect & E)): List[R] / E = handle { given s =>
      prog given new Collect[R, E] { val scope: Scope.type = Scope } map { List(_) }
    }
  }

  // this illustrates that we can use upper bounds to specify effects
  object withHandlerAndBounds {

    trait Maybe[R, E] extends Exc with Handler[Option[R], E]{
      // using some state effect
      val state: State
      type Effects <: state.effect
      private val myField = state.Field[Int](42)
      def raise(msg: String) = scope {
        myField.update(_ + 1) andThen { pure(None) }
      }
    }

    trait First[R, E] extends Amb with Handler[Option[R], E] {
      val mystate: State
      private val myField = mystate.Field[Boolean](true)

      type Effects <: mystate.effect
      def flip() = scope { for {
        b <- myField.value
        t <- resume(b)
        r <- if (t.isDefined) pure(t) else resume(false)
      } yield r }
    }

    trait Both[R, E] extends Maybe[R, E] with First[R, E]
    def both[R, E](prog: (e: Exc, a: Amb) => Control[R, e.effect & a.effect & E]): Control[Option[R], E] =
      region {
        handle[Option[R], E & State.effect] {
          val both = new Both[R, E & State.effect] {
            val state: State.type = State
            val mystate: State.type = State
            val scope: Scope.type = Scope
            type Effects = State.effect & E
          }
          prog(both, both) map { r => Some(r) }
        }
      }
  }


}
package effekt

// The preferred API: prompt composition
// While Handler[R, E] is nice for parsers that don't have dependencies
// to other effects, type members are better suited for this use case
// (see Stateful, or parsers.BreadthFirst2)
//
// update: we move away from prompt composition for a more uniform interface
// (more similar to JavaEffekt and untyped ScalaEffekt)

// since path dependency on trait-parameters is currently buggy (https://github.com/lampepfl/dotty/issues/5636),
// we can't define the dependency on other effects via parameters. So, instead
// of
//    trait MyHandler[R, E](val exc: Exc) extends Handler[R, E & exc.effect] { ... }
// we have to write
//    trait MyHandler[R, E] extends Effectful {
//      type Result  = R
//      type Effects = E & exc.effect
//    }

// For uniformity, we give up prompt composition and favor inheritance here.
// However, this way we don't guarantee accidental handling with the _same_ handler!
trait Handler[R, E] extends Prompt {
  type effects // needs to be set by the implementor

  // that's considered implementation details
  type Result = E
  type Effects = effects


  def unit: R => E / effects
  def use[A](body: CPS[A, E / effects]): A / effect = Control.use(this) { body }

  def handle[H >: this.type <: Eff](prog: given (h: H) => R / (h.effect & effects)): E / effects =
    Control.delimitCont(this) { prog given this flatMap unit }
}

object Handler {
  trait Basic[R, FX] extends Handler[R, R] {
    type effects = FX
    def unit = r => pure(r)
  }
}

trait StatefulHandler[R, E] extends Prompt with State {
  type effects

  type Result = E
  type Effects = state & effects

  def unit: R => E / (state & effects)
  def use[A](body: CPS[A, E / (state & effects)]): A / effect = Control.use(this) { body }

  def handle[H >: this.type <: Eff](prog: given (h: H) => R / (h.effect & effects)): E / effects =
    Control.delimitState(this) {
      Control.delimitCont(this) {
        prog given this flatMap unit
      }
    }
}
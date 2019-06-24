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
trait Handler[R, E] extends Eff {
  type Effects // needs to be set by the implementor

  val prompt = new Prompt[E, Effects] {}
  type effect = prompt.type

  def unit: R => E / Effects
  def use[A](body: CPS[A, E / Effects]): A / effect = Control.shift(prompt) { body }

  def handle[H >: this.type <: Eff](prog: given (h: H) => R / (h.effect & Effects)): E / Effects =
    Control.resetWith(prompt) { prog given this flatMap unit }
}

object Handler {
  trait Basic[R, FX] extends Handler[R, R] {
    type Effects = FX
    def unit = r => pure(r)
  }
}

trait StatefulHandler[R, E] extends Eff {
  type Effects

  val state  = new State {}
  val prompt = new Prompt[E, state.type & Effects] {}

  type effect = prompt.type

  def unit: R => E / (state.type & Effects)

  def Field[T](value: T) = state.Field(value)
  def use[A](body: CPS[A, E / (state.type & Effects)]): A / effect = Control.shift(prompt) { body }

  def handle[H >: this.type <: Eff](prog: given (h: H) => R / (h.effect & Effects)): E / Effects =
    Control.delimitState(state) {
      Control.resetWith(prompt) {
        prog given this flatMap unit
      }
    }
}
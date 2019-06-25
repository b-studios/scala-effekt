package object effekt {

  // Marker trait to say: "this type is effectful"
  trait Eff { type effect }

  // Marker trait to say: "this type is stateful"
  trait Stateful { type state }

  // we use intersection types to track effects, so Pure = Top
  type Pure = Any

  // A type DSL to construct implicit, dependent function types
  // Currently effectively blocked by https://github.com/lampepfl/dotty/issues/5288
  type /[+A, -E] = Control[A, E]

  type The[A] = A & Singleton

  // The usual suspects
  // ===
  final def pure[A](a: => A): A / Pure = new Trivial(a)

  final def resume[A, R](a: A): CPS[A, R] = given k => k(a)

  type CPS[A, R] = given (A => R) => R

  final def run[A](c: A / Pure): A = Result.trampoline(c(ReturnCont()))

  // delimited control
  // ===

  // Just a marker trait used by the delimcc implementation
  trait Prompt[Result, Effects]

  def shift[A, R, FX](p: Prompt[R, FX])(body: CPS[A, R / FX]): A / p.type = Control.shift(p)(body)
  def reset[R, FX](prog: (p: Prompt[R, FX]) => R / (p.type & FX)): R / FX = {
    val p = new Prompt[R, FX] {}
    Control.resetWith(p) { prog(p) }
  }

  // delimited dynamic state
  // ===
  def region[R, E](prog: (s: State) => R / (s.type & E)): R / E = {
    val s = new State {}
    Control.delimitState(s) { prog(s) }
  }

  // internally we ignore the effects
  private[effekt] type Frame[-A, +R] = A => Control[R, _]
}

package object effekt {

  // we use intersection types to track effects, so Pure = Top
  type Pure = Any

  // A type DSL to construct implicit, dependent function types
  // Currently effectively blocked by https://github.com/lampepfl/dotty/issues/5288
  type /[+A, -E] = Control[A, E]

  // we drop implicit from the function types to improve type inference for now.
  type using[+A, -H]  = [-Effects] => (h: H) => A / (h.type & Effects)
  type and[C[-_], -H] = [-Effects] => (h: H) => C[h.type & Effects]
  type also[C[-_], -E] = [-Effects] => C[E & Effects]
  type Prog[C[-_]] = C[Any]

  type CPS[A, R] = implicit (A => R) => R

  final def run[A](c: A / Pure): A = c.run

  // that is Prog[h.Res using h.type also h.Effects] but the syntactic sugar impedes
  // type inference with implicit function types
  final def handle(p: Prompt)(f: p.type => p.Result / (p.type & p.Effects)): p.Result / p.Effects =
    Control.handle(p)(h => f(h))

  final def handling[R, E](f: (p: Handler[R, E]) => R / (p.type & E)): R / E = {
    val p = new Handler[R, E] { }
    Control.handle(p)(h => f(h))
  }

  final def pure[A](a: => A): A / Pure = new Trivial(a)

  final def resume[A, R](a: A): CPS[A, R] = implicit k => k(a)


  // capture continuations
  // ===
  @scala.annotation.implicitNotFound("No prompt found for 'use'. Maybe you forgot to handle the effect?")
  trait Prompt {
    type Result
    type Effects
  }

  trait Handler[R, E] extends Prompt {
    type Result = R
    type Effects = E
    type effect = this.type

    def use[A](body: CPS[A, Result / Effects]): A / this.type = Control.use(this) { body }

    def handle(f: this.type => Result / (effect & Effects)): Result / Effects  =
      Control.handle(this) { h => f(h) }

    def apply(f: this.type => Result / (effect & Effects)): Result / Effects = handle(f)
  }

  def use[A](implicit p: Prompt) = ContinuationScope[A, p.type](p)

  // this complication is only necessary since we can't write `use {}` and have p inferred
  // as `use(p) {}`. So we write `use in {}` to mean `use(p) in {}`
  // In summary, we use value inference to guide type inference.
  case class ContinuationScope[A, P <: Prompt](p: P) {
    def in(body: CPS[A, p.Result / p.Effects]): A / p.type = Control.use(p) { body }
    def apply(body: CPS[A, p.Result / p.Effects]): A / p.type = in(body)
  }

  // internally we ignore the effects
  private[effekt] type Frame[-A, +R] = A => Control[R, _]
}

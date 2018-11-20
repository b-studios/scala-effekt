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
  final def handle(h: Handler)(f: h.type => h.Res / (h.type & h.Effects)): h.Res / h.Effects =
    h.handle(f)

  final def handling[R, E](f: (p: Handler.Type[R, E]) => R / (p.type & E)): R / E = {
    val p = new Handler { type Res = R; type Effects = E }
    Control.handle(p)(h => f(h))
  }

  final def pure[A](a: => A): A / Pure = new Trivial(a)

  final def resume[A, R](a: A): CPS[A, R] = implicit k => k(a)

  // also add type parameters to Handler to help contravariant type inference

  // capture continuations
  // ===
  @scala.annotation.implicitNotFound("No prompt found for 'use'. Maybe you forgot to handle the effect?")
  trait Handler {
    type Res
    type Effects

    def use[A](body: CPS[A, Res / Effects]): A / this.type = Control.use(this) { body }

    def handle(f: this.type => this.Res / (this.type & this.Effects)): this.Res / this.Effects  =
      Control.handle(this) { h => f(h) }

    def apply(f: this.type => Res / (this.type & Effects)): Res / Effects = handle(f)
  }
  object Handler {
    trait Base[R, E] extends Handler { type Res = R; type Effects = E }
    type Type[+R, -E] = Handler { type Res <: R; type Effects >: E }
  }
  def use[A](implicit p: Handler) = ContinuationScope[A, p.type](p)

  // this complication is only necessary since we can't write `use {}` and have p inferred
  // as `use(p) {}`. So we write `use in {}` to mean `use(p) in {}`
  // In summary, we use value inference to guide type inference.
  case class ContinuationScope[A, P <: Handler](p: P) {
    def in(body: CPS[A, p.Res / p.Effects]): A / p.type = Control.use(p) { body }
    def apply(body: CPS[A, p.Res / p.Effects]): A / p.type = in(body)
  }

  // internally we ignore the effects
  private[effekt] type Frame[-A, +R] = A => Control[R, _]
}

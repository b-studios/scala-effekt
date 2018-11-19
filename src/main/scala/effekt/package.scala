package object effekt {

  // we use intersection types to track effects, so Pure = Top
  type Pure = Any

  // A type DSL to construct implicit, dependent function types
  // Currently effectively blocked by https://github.com/lampepfl/dotty/issues/5288
  type /[+A, -E] = Control[A] { type Effects >: E }
  type using[+A, H]  = [-Effects] => implicit (h: H) => A / (h.type & Effects)
  type and[C[-_], H] = [-Effects] => implicit (h: H) => C[h.type & Effects]
  type Prog[C[-_]] = C[Any]

  type CPS[A, R] = implicit (A => R) => R

  final def run[A](c: A / Pure): A = c.run

//  final def handle(h: Handler)(f: h.R using h.type): Control[h.Res] = ??? //h.handle(f)

  final def handling[R, E](f: implicit (p: Handler.Type[R, E]) => R / (p.type & E)): R / E = {
    val p = new Handler { type Res = R; type Effects = E }
    // TODO fix this cast later
    Control.handle(p)(f.asInstanceOf[implicit p.type => p.Res / (p.type & p.Effects)])
  }

  final def pure[A](a: => A): A / Pure = new Trivial(a)

  final def resume[A, R](a: A): CPS[A, R] = implicit k => k(a)

  // capture continuations
  // ===
  @scala.annotation.implicitNotFound("No prompt found for 'use'. Maybe you forgot to handle the effect?")
  trait Handler {
    type Res
    type Effects

    def use[A](body: CPS[A, Res / Effects]): A / this.type = Control.use(this) { body }

    def handle(f: implicit this.type => Res / (this.type & Effects)): Res / Effects  =
      Control.handle(this) { f(this) }

    def apply(f: implicit this.type => Res / (this.type & Effects)): Res / Effects = handle(f)
  }
  object Handler {
    type Type[R, E] = Handler { type Res = R; type Effects = E }
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
  private[effekt] type Frame[-A, +R] = A => Control[R]
}

package object effekt {

  /**
   * Type alias for convenient use of capabilities
   *
   * @tparam E the effect signature to use
   */
  private[effekt] type Frame[-A, +B] = A => Control[B]

  type using[-A, +E] = implicit E => Control[A]
  type and[-A, +E] = implicit E => A

  type CPS[A, E] = implicit (A => Control[E]) => Control[E]

  final def run[A](c: Control[A]): A = c.run()

  final def handle(h: Handler)(f: h.R using h.type): Control[h.Res] = h.handle(f)

  final def handling[R0](f: R0 using Prompt { type Res = R0 }): Control[R0] =
    Control.handle(new Prompt { type Res = R0 })(f)

  implicit final def pure[A](a: => A): Control[A] = new Trivial(a)

  final def resume[A, Res](a: A): CPS[A, Res] = implicit k => k(a)

  // capture continuations
  // ===
  // TODO rename Prompt to Delimiter?
  trait Prompt { type Res }
  def use[A](implicit p: Prompt) = ContinuationScope[A, p.type](p)

  // this complication is only necessary since we can't write `use {}` and have p inferred
  // as `use(p) {}`. So we write `use in {}` to mean `use(p) in {}`
  case class ContinuationScope[A, P <: Prompt](p: P) {
    def in(body: CPS[A, p.Res]): Control[A] = Control.use(p) { body }
    def apply(body: CPS[A, p.Res]): Control[A] = in(body)
  }

  // ambient state
  // ===
  trait State[S] { def get(): Control[S]; def put(s: S): Control[Unit] }
  def stateful[S, R](init: S)(body: State[S] => Control[R]): Control[R] = {
    val state = new State[S] with Stateful[S] {
      private var state: S = init
      def get(): Control[S] = state
      def put(s: S): Control[Unit] = state = s
      def onSave(): S = state
      def onLoad(s: S): Unit = state = s
    }

    Control.stateful(state) { body }
  }
}

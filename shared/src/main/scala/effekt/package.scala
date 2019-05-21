package object effekt {

  private[effekt] type Frame[-A, +B] = A => Control[B]

  @scala.annotation.implicitNotFound("No prompt found for 'use'. Maybe you forgot to handle the effect?")
  trait Prompt[Res]

  type using[+A, -E] = E => Control[A]

  type CPS[+A, E] = (A => Control[E]) => Control[E]

  final def run[A](c: Control[A]): A = c.run()

  final def handle[R, Res](h: Handler[R, Res])(f: R using h.type): Control[Res] = h.handle(f)

  final def handling[Res](f: Res using Prompt[Res]): Control[Res] = Control.handle(new Prompt[Res] {})(f)

  final def pure[A](a: => A): Control[A] = new Trivial(a)

  final def use[A, Res](body: CPS[A, Res])(implicit p: Prompt[Res]) = Control.use(p) { body }

  // ambient state
  // ===
  def stateful[S, R](init: S)(body: Stateful[S] => Control[R]): Control[R] = {
    val state = new Stateful[S] {
      private var state: S = init
      def get(): S = state
      def put(s: S): Unit = state = s
    }

    Control.stateful(state) { body }
  }
}

package object effekt {

  /**
   * Type alias for convenient use of capabilities
   *
   * @tparam E the effect signature to use
   */
  private[effekt] type Frame[-A, +B] = A => Control[B]

  type using[+A, -E] = given E => Control[A]
  type and[+A, -E] = given E => A

  type CPS[A, E] = given (A => Control[E]) => Control[E]

  type StatefulCPS[A, E, S] = given ((A, S) => Control[E]) => S => Control[E]

  final def run[A](c: Control[A]): A = c.run()

  final def handle[R, Res](h: Handler[R, Res])(f: R using h.type): Control[Res] = h.handle(f)

  final def handling[Res](f: Res using Prompt[Res]): Control[Res] = Control.handle(new Prompt[Res] {})(f)

  final def pure[A](a: => A): Control[A] = new Trivial(a)

  final def resume[A, Res](a: A): CPS[A, Res] = given k => k(a)
  final def resume[A, Res, S](a: A, s: S) given (k: ((A, S) => Control[Res])): Control[Res] = k(a, s)

  // capture continuations
  // ===
  @scala.annotation.implicitNotFound("No prompt found for 'use'. Maybe you forgot to handle the effect?")
  trait Prompt[Res]
  def use[A, Res](body: CPS[A, Res]) given (p: Prompt[Res]) = Control.use(p) { body }

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

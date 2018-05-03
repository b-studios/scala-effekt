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

  final def handle(h: Handler)(f: h.R using h.type): Control[h.Res] = Control.handle(h)(f)

  implicit final def pure[A](a: => A): Control[A] = new Trivial(a)

  final def resume[A, Res](a: A): CPS[A, Res] = implicit k => k(a)
}

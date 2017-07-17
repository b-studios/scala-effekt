package object effekt {

  /**
   * Type alias for convenient use of capabilities
   *
   * @tparam E the effect signature to use
   */
  type Use[E] = Capability {
    val effect: E with Handler
  }
  private[effekt] type Frame[-A, +B] = A => Control[B]

  type CPS[A, Res] = (A => Control[Res]) => Control[Res]

  @inline final def pure[A](a: A): Control[A] = Control.pure(a)

  @inline final def use[A](c: Capability)(f: CPS[A, c.Res]): Control[A] =
    Control.use[A](c)(f)

  @inline final def handle(h: Handler)(f: Use[h.type] => Control[h.R]): Control[h.Res] =
    Control.handle(h)(f)
}

package object effekt {

  /**
   * Type alias for convenient use of capabilities
   *
   * @tparam E the effect signature to use
   */
  type Use[E <: Eff] = Capability {
    val effect: E with Handler
  }

  private[effekt] type Frame[-A, +B] = A => Control[B]

  @inline final def pure[A](a: A): Control[A] = Control.pure(a)

  @inline final def use[A](c: Capability)(f: c.effect.Op[A]): Control[A] = Control.use[A](c)(f)

  @inline final def handle(h: Handler)(init: h.State)(
    f: Capability { val effect: h.type } => Control[h.R]
  ): Control[h.Res] = Control.handle(h)(init)(f)

  @inline final def handle[R, Res](h: Handler.Basic[R, Res])(
    f: Capability { val effect: h.type } => Control[R]
  ): Control[Res] = Control.handle(h)(())(f)
}

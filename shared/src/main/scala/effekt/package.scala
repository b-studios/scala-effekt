package object effekt {

  /**
   * Type alias for convenient use of capabilities
   *
   * @tparam E the effect signature to use
   */
  type Use[E <: Eff] = Capability {
    val effect: E
  }
  private[effekt] type Frame[-A, +B] = A => Control[B]

  @inline final def pure[A](a: A): Control[A] = Control.pure(a)

  @inline final def use[A](c: Capability)(
    f: (A => Control[c.Res]) => Control[c.Res]
  ): Control[A] = Control.use[A](c)(f)

  @inline final def handle[R, Res](e: Handler[R, Res])(
    f: Capability { val effect: e.type } => Control[R]
  ): Control[Res] = Control.handle(e)(f)
}

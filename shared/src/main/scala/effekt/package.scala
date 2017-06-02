package effekt

trait EffektApi {

  /**
   * Type alias for convenient use of capabilities
   *
   * @tparam E the effect signature to use
   */
  type Use[E <: Eff] = Capability {
    val effect: E
  }

  @inline final def pure[A](a: A): Control[A] = Control.pure(a)

  @inline final def use[A](c: Capability)(
    f: c.effect.State => (A => c.effect.State => Control[c.Res]) => Control[c.Res]
  ): Control[A] = Control.use[A](c)(f)

  @inline final def handle[R](e: Eff)(init: e.State)(
    f: Capability {val effect: e.type} => Control[R]
  ): Control[e.Out[R]] = Control.handle[R](e)(init)(f)

  @inline final def handle[R](e: Eff { type State = Unit })(
    f: Capability {val effect: e.type} => Control[R]
  ): Control[e.Out[R]] = Control.handle[R](e)(())(f)
}

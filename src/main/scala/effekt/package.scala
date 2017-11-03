package object effekt {

  /**
   * Type alias for convenient use of capabilities
   *
   * @tparam E the effect signature to use
   */
  private[effekt] type Frame[-A, +B] = A => Control[B]
  private[effekt] type Capability = Cap[_]

  type using[A, E] = implicit Cap[E] => Control[A]
  type and[A, E] = implicit Cap[E] => A

  type CPS[A, Res] = implicit (A => Control[Res]) => Control[Res]

  final def run[A](c: Control[A]): A = c.run()

  final def pure[A](a: A): Control[A] = Control.pure(a)

  final def use[A](c: Cap[_])(f: CPS[A, c.Res]): Control[A] =
    Control.use[A](c)(f)

  final def handle(h: Handler)(f: Cap[h.type] => Control[h.R]): Control[h.Res] =
    Control.handle(h)(f)

  final def resume[A, Res](a: A): CPS[A, Res] = implicit k => k(a)
}

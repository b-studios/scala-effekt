import scala.util.escape._

package object effekt {

  type Use[E <: Eff] = Capability {
    val effect: E
  }

  private[effekt] type H[C <: Capability] = Handler { val prompt: C }
  private[effekt] type Frame[-A, +B] = A => Control[B]

  @inline final def pure[A](a: A): Control[A] = Control.pure(a)

  @inline final def use[A](@local c: Capability)(
    f: c.effect.State => (A => c.effect.State => Control[c.Res]) -> Control[c.Res]
  ): Control[A] = c.use(f)

  @inline final def handle[R](e: Eff)(init: e.State)(
    f: Capability {val effect: e.type} -> Control[R]
  ): Control[e.Out[R]] = Control.handle[R](e)(init)(f)

  @inline final def handle[R](e: Eff { type State = Unit })(
    f: Capability {val effect: e.type} -> Control[R]
  ): Control[e.Out[R]] = Control.handle[R](e)(())(f)
}

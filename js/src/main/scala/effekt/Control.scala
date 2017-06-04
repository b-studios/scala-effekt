package effekt

import scala.scalajs.js
import js.annotation._

/**
 * See documentation of jvm/Control
 */
@js.native
trait Control[+A] extends js.Object { outer =>

  def run(): A = js.native

  def map[B](f: A -> B): Control[B] = js.native

  def flatMap[B](f: A -> Control[B]): Control[B] = js.native
}

object Control {

  final def pure[A](a: A): Control[A] = ControlJS.pure[A](a)

  final def use[A](c: Capability)(
    f: c.effect.State => (A => c.effect.State => Control[c.Res]) => Control[c.Res]
  ): Control[A] = {
    def f2: (c.effect.State, js.Function2[A, c.effect.State, Control[c.Res]]) => Control[c.Res] =
      (s, k) => f(s)(a => s => k(a, s))
    ControlJS.use[A](c)(f2)
  }

  final def handle[R](e: Eff)(init: e.State)(
    f: Capability { val effect: e.type } => Control[R]
  ): Control[e.Out[R]] = ControlJS.handle[R](e)(init)(f)

}

@js.native
@JSGlobal
private[effekt] object ControlJS extends js.Object {

  final def pure[A](a: A): Control[A] = js.native

  def use[A](c: Capability)(
    f: js.Function2[c.effect.State, js.Function2[A, c.effect.State, Control[c.Res]], Control[c.Res]]
  ): Control[A] = js.native

  def handle[R](e: Eff)(init: e.State)(
    f: js.Function1[Use[e.type], Control[R]]
  ): Control[e.Out[R]] = js.native

}

@JSExportTopLevel("JSControl")
object JSControl {
  @JSExport
  def unit[A](e: Eff)(s: e.State, a: A) = e.unit(s, a)
}
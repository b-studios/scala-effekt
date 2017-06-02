package effekt

import scala.scalajs.js
import js.annotation._

/**
 * See documentation of jvm/Control
 */
@js.native
trait Control[+A] extends js.Object { outer =>

  def run(): A = js.native

  def map[B](f: A => B): Control[B] = js.native

  def flatMap[B](f: A => Control[B]): Control[B] = js.native
}

@js.native
@JSGlobal
object Control extends js.Object {

  final def pure[A](a: A): Control[A] = js.native

  final def use[A](c: Capability)(
    f: c.effect.State => (A => c.effect.State => Control[c.Res]) => Control[c.Res]
  ): Control[A] = js.native

  final def handle[R](e: Eff)(init: e.State)(
    f: Capability { val effect: e.type } => Control[R]
  ): Control[e.Out[R]] = js.native
}

package effekt

import scala.scalajs.js
import js.annotation._

/**
 * A wrapper around an effect handler that serves as capability to use the effect.
 *
 * Instances of [[Capability]] should only be obtained by calling `handle`.
 */
@js.native
sealed trait Capability extends js.Object {

  /**
   * the handler implementation
   */
  val effect: Eff

  /**
   * the answertype
   */
  type R

  /**
   * shorthand for the answertype in the effect interpretation
   */
  type Res = effect.Out[R]
}
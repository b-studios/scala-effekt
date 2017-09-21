package effekt

/**
 * A wrapper around an effect handler that serves as capability to use the effect.
 *
 * Instances of [[Capability]] should only be obtained by calling `handle`.
 */
sealed trait Capability {

  /**
   * the handler implementation
   */
  val effect: Handler


  /**
   * shorthand for the answertype in the effect interpretation
   */
  type Res = effect.Res
}

object Capability {
  // only Control.handle is allowed to create new capabilities
  private[effekt] def apply(h: Handler): Capability { val effect: h.type } =
    new Capability { override val effect: h.type = h }
}
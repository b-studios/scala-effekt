package effekt

/**
 * A wrapper around an effect handler that serves as capability to use the effect.
 *
 * Instances of [[Cap]] should only be obtained by calling `handle`.
 */
sealed trait Cap[+E] {

  /**
   * the handler implementation
   */
  val handler: Handler with E

  type Res = handler.Res
}

object Cap {
  // only Control.handle is allowed to create new capabilities
  private[effekt] def apply[Res0](h: Handler): Cap[h.type] =
    new Cap[h.type] { override val handler: h.type = h }
}
package effekt

sealed trait Capability {

  // the handler implementation
  val effect: Eff

  // the answertype
  type R

  // shorthand for the answertype in the effect interpretation
  type Res = effect.Out[R]
}

object Capability {
  // only Control.handle is allowed to create new capabilities
  private[effekt] def apply[R0](e: Eff): Capability { val effect: e.type; type R = R0 } =
    new Capability {
      type R = R0
      override val effect: e.type = e
    }
}
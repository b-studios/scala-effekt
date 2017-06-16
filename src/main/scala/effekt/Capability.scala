package effekt

import scala.util.escape._

sealed trait Capability { c =>

  // the handler implementation
  val effect: Eff

  // the answertype
  type R

  // shorthand for the answertype in the effect interpretation
  type Res = effect.Out[R]

  private[effekt] def use[A](
    f: effect.State => (A => effect.State => Control[Res]) -> Control[Res]
  ): Control[A] = new Control[A] {

    def apply[R](k: MetaCont[A, R]): R = {

      // slice the meta continuation in three parts
      val (init, h, tail) = k splitAt c

      val localCont: A => c.effect.State => Control[c.Res] =
        a => updatedState => new Control[c.Res] {
          def apply[R2](k: MetaCont[c.Res, R2]): R2 = {

            // create a copy of the handler with the very same prompt but
            // updated state
            val updatedHandler = h updateWith updatedState

            // as in shift and shift0 we repush the prompt, but here
            // we also internally update the contained state.
            val repushedPrompt = init append HandlerCont(updatedHandler, k)

            //              println("return %-15s k = %s".format(s"${actionName} = ${a}", repushedPrompt))

            // invoke assembled continuation
            repushedPrompt(a)
          }
        }

      // run f with the current state to obtain a value of type A and an
      // updated copy of the state.
      val handled: Control[c.Res] = f(h.state)(localCont)

      // continue with tail
      handled(tail)
    }
  }
}

object Capability {
  // only Control.handle is allowed to create new capabilities
  private[effekt] def apply[R0](e: Eff): Capability { val effect: e.type; type R = R0 } =
    new Capability {
      type R = R0
      override val effect: e.type = e
    }
}

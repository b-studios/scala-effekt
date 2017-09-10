package effekt

/**
 * The effect monad, implementing delimited control.
 *
 * Effectful programs that use the effect `E <: Eff` and return
 * `A` typically have the type
 *
 * {{{
 *    implicit Use[E] => Control[A]
 * }}}
 *
 * Given the capability to use the effect `E`, the result of
 * type `A` is interpreted in the `Control` and can be obtained
 * using the method `run()`. It is important to know that calling
 * `run` before all effects have been handled will lead to a
 * runtime exception.
 *
 * {{{
 *  // safe use of `run`
 *  handle(ambHandler) { implicit a => flip() }.run()
 *
 *  // unsafe use of `run`
 *  handle(ambHandler) { implicit a => flip().run() }
 * }}}
 *
 * =Implementation Details=
 *
 * The Control` monad itself is a specialized variant of the
 * multiprompt delimited control monad, as presented in:
 *
 *     A Monadic Framework for Delimited Continuations [[http://www.cs.indiana.edu/~sabry/papers/monadicDC.pdf PDF]]
 *
 * Capabilities, obtained by the `handle` primitive act as
 * prompt markers.
 *
 * @tparam A the type of the resulting value which is eventually
 *           computed within the control monad.
 */
sealed trait Control[+A] { outer =>

  /**
   * Runs the computation to yield an A
   *
   * Attention: It is unsafe to run control if not all effects have
   *            been handled!
   */
  def run(): A = Result.trampoline(Impure(this, ReturnCont(identity[A])))

  def map[B](f: A => B): Control[B] = new Control[B] {
    override def toString = s"Map"
    def apply[R](k: MetaCont[B, R]): Result[R] = Impure(outer, k map f)
  }

  def flatMap[B](f: A => Control[B]): Control[B] = new Control[B] {
    override def toString = s"Flatmap"
    def apply[R](k: MetaCont[B, R]): Result[R] = Impure(outer, k flatMap f)
  }

  private[effekt] def apply[R](k: MetaCont[A, R]): Result[R]
}

private[effekt]
final class Trivial[+A](a: A) extends Control[A] {
  def apply[R](k: MetaCont[A, R]): Result[R] = k(a)

  override def run(): A = a

  override def toString = s"Trivial($a)"
}

private[effekt]
trait Handler extends Serializable { outer =>

  val prompt: Capability
  val state: prompt.effect.State
  type Res = prompt.Res

  val cleanupActions: List[() => Unit]

  final def cleanup() = for (c <- cleanupActions) { c() }

  def updateWith(s: prompt.effect.State): Handler {val prompt: outer.prompt.type} =
    new Handler {
      val prompt: outer.prompt.type = outer.prompt
      val state = s
      val cleanupActions = outer.cleanupActions
    }
  def updateWith(c: List[() => Unit]): Handler {val prompt: outer.prompt.type} =
    new Handler {
      val prompt: outer.prompt.type = outer.prompt
      val state = outer.state
      val cleanupActions = c
    }
  def removeCleanup: Handler {val prompt: outer.prompt.type} = updateWith(Nil)
  def prependCleanup(c: List[() => Unit]): Handler {val prompt: outer.prompt.type} =
    updateWith(c ::: cleanupActions)
}

object Control {

  private[effekt] def pure[A](a: A): Control[A] = new Trivial(a)

  // f receives an updated copy of E, reflecting the new state of the
  // parametrized handler and a continuation that takes both the
  // next value and again an updated handler.
  //
  // Operationally, it
  // (1) slices the meta continuation at point E, obtaining cont k up to E and
  //     the current version e2 of E
  // (2) calls f with the current version of E to obtain an A and yet a new version
  //     e3 of E.
  // (3) splices in k and pushes e3 as prompt
  private[effekt] final def use[A](c: Capability)(
    f: c.effect.State => (A => c.effect.State => Control[c.Res]) => Control[c.Res]
  ): Control[A] = new Control[A] {

    override def toString = s"use($c, definition site)"

    def apply[R](k: MetaCont[A, R]): Result[R] = {

      // slice the meta continuation in three parts
      val (init, h, tail) = k splitAt c

      val localCont: A => c.effect.State => Control[c.Res] =
        a => updatedState => new Control[c.Res] {

          override def toString = s"use($c, $a, continued)"

          def apply[R2](k: MetaCont[c.Res, R2]): Result[R2] = {

            // create a copy of the handler with the very same prompt but
            // updated state
            val updatedHandler = h updateWith updatedState

            // as in shift and shift0 we repush the prompt, but here
            // we also internally update the contained state.
            val repushedPrompt = init append HandlerCont(updatedHandler, k)

            // invoke assembled continuation
            // for now wrapped in Trivial to allow trampolining and
            // ressource cleanup
            Impure(pure(a), repushedPrompt)
          }
        }

      // run f with the current state to obtain a value of type A and an
      // updated copy of the state.
      val handled: Control[c.Res] = f(h.state)(localCont)

      // continue with tail
      Impure(handled, tail)
    }
  }

  private[effekt] final def handle[R](e: Eff)(init: e.State)(
    f: Capability { val effect: e.type } => Control[R]
  ): Control[e.Out[R]] = {

    // produce a new prompt
    val p = Capability[R](e)

    // construct handler from prompt and initial state
    val h = new Handler {
      val prompt: p.type = p
      val state = init
      val cleanupActions = List(e.cleanup)
    }

    new Control[e.Out[R]] {

      override def toString = s"handle($p, definition site)"

      def apply[R2](k: MetaCont[e.Out[R], R2]): Result[R2] = {
        // extract new state
        val c = f(p).flatMap { a =>
          new Control[e.Out[R]] {

            override def toString = s"handle($p, $a, unit and updated state)"

            def apply[R3](k: MetaCont[e.Out[R], R3]): Result[R3] = {
              (k: @unchecked) match {
                // Invariant: The last continuation on the metacont stack is a HandlerCont for p
                case HandlerCont(h2: H[p.type] @unchecked, k2) => {
                  // after lifting a into the result type of the handler, perform
                  // a resource cleanup.
                  val res = e.unit[R](h2.state, a)
                  h2.cleanup()

                  // now continue
                  // for now wrapped in Trivial to allow trampolining and
                  // ressource cleanup
                  Impure(pure(res), k2)
                }
              }
            }
          }
        }
        Impure(c, HandlerCont(h, k))
      }
    }
  }
}

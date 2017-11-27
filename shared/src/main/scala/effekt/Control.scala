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
 * type `A` is interpreted in `Control` and can be obtained
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

  def map[B](f: A => B): Control[B] = Computation { k => Impure(outer, k map f) }

  def flatMap[B](f: A => Control[B]): Control[B] = Computation { k => Impure(outer, k flatMap f) }

  private[effekt] def apply[R](k: MetaCont[A, R]): Result[R]
}

private[effekt]
final class Trivial[+A](a: A) extends Control[A] {
  // NOTE don't wrap this in a trampolining call, since this will
  //      cause nontermination.
  def apply[R](k: MetaCont[A, R]): Result[R] = k(a)

  override def run(): A = a

  override def toString = s"Trivial($a)"
}

private[effekt]
sealed trait ω

private[effekt]
final case class Computation[+A](body: MetaCont[A, ω] => Result[ω]) extends Control[A] {
  def apply[R](k: MetaCont[A, R]): Result[R] =
    body(k.asInstanceOf[MetaCont[A, ω]]).asInstanceOf[Result[R]]
}


private[effekt]
trait HandlerFrame extends Serializable { outer =>

  // used as prompt marker to identify the frame and to provide the types Res and State
  val cap: Capability

  type Res   = cap.Res
  type State = cap.handler.State

  val state: State
  val cleanupActions: List[() => Unit]

  final def cleanup() = for (c <- cleanupActions) { c() }

  def updateWith(s: State) = HandlerFrame(cap)(s, cleanupActions)
  def updateWith(c: List[() => Unit]) = HandlerFrame(cap)(state, c)
  def removeCleanup = updateWith(Nil)
  def prependCleanup(c: List[() => Unit]) = updateWith(c ::: cleanupActions)
}

private[effekt]
object HandlerFrame {
  type Aux[C <: Capability] = HandlerFrame { val cap: C }

  def apply(c: Capability)(st: c.handler.State, cl: List[() => Unit]): HandlerFrame { val cap: c.type } =
    new HandlerFrame { val cap: c.type = c; val state = st; val cleanupActions = cl; }
}

object Control {

  private[effekt] def pure[A](a: A): Control[A] = new Trivial(a)

  private[effekt] final def use[A](c: Capability)(
    f: c.handler.State => (A => c.handler.State => Control[c.Res]) => Control[c.Res]
  ): Control[A] = Computation { k =>

    // slice the meta continuation in three parts
    val (init, h, tail) = k splitAt c

    val localCont: A => c.handler.State => Control[c.Res] =
      a => updatedState => Computation[c.Res] { k =>

        // create a copy of the handler with the very same prompt but
        // updated state
        val updatedHandler = h updateWith updatedState

        // as in shift we repush the prompt, but here
        // we also internally update the contained state.
        val repushedPrompt = init append HandlerCont(updatedHandler, k)

        // invoke assembled continuation
        // for now wrapped in pure to allow trampolining and
        // ressource cleanup
        Impure(pure(a), repushedPrompt)
      }

    // run f with the current state to obtain a value of type A and an
    // updated copy of the state.
    val handled: Control[c.Res] = f(h.state)(localCont)

    // continue with tail
    Impure(handled, tail)
  }

  private[effekt] final def handle(h: Handler)(init: h.State)(
    f: Capability { val handler: h.type } => Control[h.R]
  ): Control[h.Res] = {

    // produce a new capability
    val p = Capability(h)

    Computation { k =>

      // extract new state
      val c = f(p).flatMap { a =>
        Computation[h.Res] { k => (k: @unchecked) match {
            // Invariant: The last continuation on the metacont stack is a HandlerCont for p
            case HandlerCont(h2: HandlerFrame.Aux[p.type] @unchecked, k2) => {
              // after lifting a into the result type of the handler, perform
              // a resource cleanup.
              val res = h.unitState(h2.state)(a)
              h2.cleanup()

              // now continue
              // for now wrapped in Trivial to allow trampolining and
              // ressource cleanup
              Impure(res, k2)
            }
          }
        }
      }

      // initial handler frame
      val hf = HandlerFrame(p)(init, List(h._finally))

      Impure(c, HandlerCont(hf, k))
    }
  }
}

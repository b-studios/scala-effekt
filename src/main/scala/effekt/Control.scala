package effekt

/**
 * The effect monad, implementing delimited control.
 *
 * Effectful programs that use the effect `E <: Eff` and return
 * `A` typically have the type
 *
 * {{{
 *    given Use[E] => Control[A]
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
 *  handle(ambHandler) { given a => flip() }.run()
 *
 *  // unsafe use of `run`
 *  handle(ambHandler) { given a => flip().run() }
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
  def run(): A = Result.trampoline(apply(ReturnCont(identity)))

  def map[B](f: A => B): Control[B] = new Control[B] {
    def apply[R](k: MetaCont[B, R]): Result[R] = outer(k map f)
  }

  def flatMap[B](f: A => Control[B]): Control[B] = new Control[B] {
    def apply[R](k: MetaCont[B, R]): Result[R] = outer(k flatMap f)
  }

  private[effekt] def apply[R](k: MetaCont[A, R]): Result[R]
}

private[effekt]
final class Trivial[+A](a: => A) extends Control[A] {
  def apply[R](k: MetaCont[A, R]): Result[R] = k(a)

  override def map[B](f: A => B): Control[B] = new Trivial(f(a))

  // !!! this affects side effects raised by f !!!
//  override def flatMap[B](f: A => Control[B]): Control[B] = f(a)

  override def run(): A = a
}


object Control {

  private[effekt] final def use[A](c: Prompt)(f: CPS[A, c.Res]): Control[A] =
    new Control[A] {
      def apply[R](k: MetaCont[A, R]): Result[R] = {

        val (init, tail) = k splitAt c

        val handled: Control[c.Res] = f given { a =>
          new Control[c.Res] {
            def apply[R2](k: MetaCont[c.Res, R2]): Result[R2] =
              (init append k).apply(a)
          }
        }

        // continue with tail
        Impure(handled, tail)
      }
    }

  private[effekt] final def handle(h: Prompt)(f: h.Res using h.type): Control[h.Res] =
    new Control[h.Res] {
      def apply[R2](k: MetaCont[h.Res, R2]): Result[R2] = {
        Impure(f given h, HandlerCont[h.Res, R2](h)(k))
      }
    }

  private[effekt] final def stateful[S, R](s: Stateful[S])(f: s.type => Control[R]): Control[R] =
    new Control[R] {
      def apply[R2](k: MetaCont[R, R2]): Result[R2] = {
        Impure(f(s), StateCont(s, null.asInstanceOf[S], k))
      }
    }
}
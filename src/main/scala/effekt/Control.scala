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
sealed trait Control[+A, -Effects] { outer =>

  /**
   * Runs the computation to yield an A
   */
  def run[E <: Effects](implicit erased ev: E =:= Pure): A =
    Result.trampoline(apply(ReturnCont(identity)))

  def map[B](f: A => B): B / Effects = new Control[B, Effects] {
    def apply[R](k: MetaCont[B, R]): Result[R] = outer(k map f)
  }

  def flatMap[B, E](f: A => B / E): B / (E & Effects) = new Control[B, E & Effects] {
    def apply[R](k: MetaCont[B, R]): Result[R] = outer(k flatMap f)
  }

  def withFilter(p: A => Boolean): A / Effects = flatMap {
    case a if p(a) => pure(a)
    case a => new Error(new Throwable("Could not match " + a))
  }

  private[effekt] def apply[R](k: MetaCont[A, R]): Result[R]
}

private[effekt]
final class Trivial[+A](a: => A) extends Control[A, Pure] {
  def apply[R](k: MetaCont[A, R]): Result[R] = k(a)

  override def map[B](f: A => B): B / Pure = new Trivial(f(a))

  override def run[E <: Pure](implicit erased ev: E =:= Pure): A = a
}

private[effekt]
final class Error(t: Throwable) extends Control[Nothing, Pure] {
  def apply[R](k: MetaCont[Nothing, R]): Result[R] = Abort(t)
  override def map[B](f: Nothing => B): B / Pure = this.asInstanceOf[B / Pure]
  override def flatMap[B, E](f: Nothing => B / E): B / (E & Pure) = this.asInstanceOf[B / Pure]
}


object Control {

  private[effekt] final def use[A](p: Prompt)(f: CPS[A, p.Res / p.Effects]): A / p.type =
    new Control[A, p.type] {
      def apply[R](k: MetaCont[A, R]): Result[R] = {

        val (init, tail) = k splitAt p

        val handled: Control[p.Res, p.Effects] = f { a =>
          new Control[p.Res, p.Effects] {
            def apply[R2](k: MetaCont[p.Res, R2]): Result[R2] =
              (init append k).apply(a)
          }
        }

        // continue with tail
        Computation(handled, tail)
      }
    }

  private[effekt] final def handle(p: Prompt)(f: p.type => p.Res / (p.type & p.Effects)): p.Res / p.Effects =
    new Control[p.Res, p.Effects] {
      def apply[R2](k: MetaCont[p.Res, R2]): Result[R2] = {
        Computation(f(p), HandlerCont[p.Res, R2](p)(k))
      }
    }

//  private[effekt] final def stateful[S, R](s: Stateful[S])(f: s.type => Control[R]): Control[R] =
//    new Control[R] {
//      def apply[R2](k: MetaCont[R, R2]): Result[R2] = {
//        Computation(f(s), StateCont(s, null.asInstanceOf[S], k))
//      }
//    }
}
package effekt

/**
 * The effect monad, implementing delimited control.
 *
 * Effectful programs that use the effect `E` and return
 * `A` typically have the type
 *
 * {{{
 *    implicit (e: E) => Control[A, e.type]
 * }}}
 *
 * Given the capability to use the effect `E`, the result of
 * type `A` is interpreted in the `Control` and can be obtained
 * using the method `run()`.
 * `run` can only be called after all effects are handled, that is
 * when Effects = Pure.
 *
 * {{{
 *  // safe use of `run`
 *  handle(ambHandler) { amb => amb.flip() }.run()
 *
 *  // will cause a type error
 *  handle(ambHandler) { amb => amb.flip().run() }
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
 *
 * @tparam Effects an intersection type tracking all effects that
 *                 still need to be handled.
 */
sealed trait Control[+A, -Effects] { outer =>

  def map[B](f: A => B): B / Effects = new Control[B, Effects] {
    def apply[R](k: MetaCont[B, R]): Result[R] = outer(k map f)
  }

  def flatMap[B, E](f: A => B / E): B / (E & Effects) = new Control[B, E & Effects] {
    def apply[R](k: MetaCont[B, R]): Result[R] = outer(k flatMap f)
  }

  def andThen[B, E](c: B / E): B / (E & Effects) = flatMap { _ => c }

  def foreach(f: A => Unit): Unit / Effects = map(f)

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
}

private[effekt]
final class Error(t: Throwable) extends Control[Nothing, Pure] {
  def apply[R](k: MetaCont[Nothing, R]): Result[R] = Abort(t)
  override def map[B](f: Nothing => B): B / Pure = this.asInstanceOf[B / Pure]
  override def flatMap[B, E](f: Nothing => B / E): B / (E & Pure) = this.asInstanceOf[B / Pure]
}


object Control {

  private[effekt] final def shift[A, E, FX](p: Prompt[E, FX])(f: CPS[A, E / FX]): A / p.type =
    new Control[A, p.type] {
      def apply[R](k: MetaCont[A, R]): Result[R] = {

        val (init, tail) = k splitAt p

        val handled: Control[E, FX] = f given { a =>
          new Control[E, FX] {
            def apply[R2](k: MetaCont[E, R2]): Result[R2] =
              (init append k).apply(a)
          }
        }

        // continue with tail
        Computation(handled, tail)
      }
    }

  private[effekt] final def resetWith[E, FX](p: Prompt[E, FX])(f: E / (p.type & FX)): E / FX =
    new Control[E, FX] {
      def apply[R2](k: MetaCont[E, R2]): Result[R2] = {
        Computation(f, PromptCont[E, R2](p)(k))
      }
    }

  private[effekt] final def delimitState[R, E](s: State)(f: R / (s.type & E)): R / E =
    new Control[R, E] {
      def apply[R2](k: MetaCont[R, R2]): Result[R2] = {
        Computation(f, StateCont[R, R2](s, k))
      }
    }
}

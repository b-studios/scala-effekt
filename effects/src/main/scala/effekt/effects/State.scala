package effekt
package effects

trait Reader[S] extends Eff {
  def ask[R](): S @@ R
}
trait ReaderSyntax {
  def ask[S]()(implicit u: Use[Reader[S]]): Control[S] =
    use(u)(u.effect.ask())
}
object Reader extends ReaderSyntax

trait Writer[S] extends Eff {
  def tell[R](s: S): Unit @@ R
}
trait WriterSyntax {
  def tell[S](s: S)(implicit u: Use[Writer[S]]): Control[Unit] =
    use(u)(u.effect.tell(s))
}
object Writer extends WriterSyntax

/**
 * A simple state effect.
 *
 * Can be used like:
 *
 * {{{
 *   import State._
 *   def prog(implicit s: Use[State[Int]]) = for {
 *     x <- s.value
 *     _ <- s.value = 42
 *     y <- s.value
 *   } yield (x, y)
 *
 *   handle(state[Int])(0) { implicit s => prog }
 * }}}
 */
trait State[S] extends Reader[S] with Writer[S] {
  def put[R](s: S): Unit @@ R
  def get[R](): S @@ R
  def ask[R]() = get()
  def tell[R](s: S) = put(s)
}
object State extends ReaderSyntax with WriterSyntax {
  def get[S]()(implicit u: Use[State[S]]): Control[S] =
    use(u)(u.effect.get())

  def put[S](s: S)(implicit u: Use[State[S]]): Control[Unit] =
    use(u)(u.effect.put(s))

  implicit class StateOps[S](val u: Use[State[S]]) extends AnyVal {
    def value: Control[S] = get[S]()(u)
    def value_=(s: S): Control[Unit] = put[S](s)(u)
  }

  def state[S] = new State[S] {
    type State = S
    type Out[A] = (State, A)
    def unit[A] = (s, a) => (s, a)

    def put[R](s: S) = state => resume => resume(())(s)
    def get[R]() = state => resume => resume(state)(state)
  }
}
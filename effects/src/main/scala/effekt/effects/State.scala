package effekt
package effects

trait Reader[S] extends Eff {
  def ask(): Op[S]
}
trait ReaderSyntax {
  def ask[S]()(implicit u: Use[Reader[S]]): Control[S] =
    use(u)(u.handler.ask())
}
object Reader extends ReaderSyntax

trait Writer[S] extends Eff {
  def tell(s: S): Op[Unit]
}
trait WriterSyntax {
  def tell[S](s: S)(implicit u: Use[Writer[S]]): Control[Unit] =
    use(u)(u.handler.tell(s))
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
 *   handle(state)(0) { implicit s => prog }
 * }}}
 */
trait State[S] extends Reader[S] with Writer[S] {
  def put(s: S): Op[Unit]
  def get(): Op[S]
  def ask() = get()
  def tell(s: S) = put(s)
}
object State extends ReaderSyntax with WriterSyntax {
  def get[S]()(implicit u: Use[State[S]]): Control[S] =
    use(u)(u.handler.get())

  def put[S](s: S)(implicit u: Use[State[S]]): Control[Unit] =
    use(u)(u.handler.put(s))

  implicit class StateOps[S](val u: Use[State[S]]) extends AnyVal {
    def value: Control[S] = get[S]()(u)
    def value_=(s: S): Control[Unit] = put[S](s)(u)
  }

  def state[R, S] = new Handler.Stateful[R, R, S] with State[S] {
    def unit = a => a
    def put(s: S) = state => resume => resume(())(s)
    def get() = state => resume => resume(state)(state)
  }
}
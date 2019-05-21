package effekt
package effects

trait Reader[S] {
  def ask(): Control[S]
}

trait Writer[S] {
  def tell(s: S): Control[Unit]
}


/**
 * A simple state effect.
 *
 * Can be used like:
 *
 * {{{
 *   def prog(implicit s: State[Int]) = for {
 *     x <- s.value
 *     _ <- s.value = 42
 *     y <- s.value
 *   } yield (x, y)
 *
 *   handle(state)(0) { implicit s => prog }
 * }}}
 */
trait State[S] extends Reader[S] with Writer[S] {
  def value_=(s: S): Control[Unit]
  def value: Control[S]
  def ask() = value
  def tell(s: S) = this.value = s
}
//object State extends ReaderSyntax with WriterSyntax {
//
//  implicit class StateOps[S](val u: State[S]) extends AnyVal {
//    def value: Control[S] = get[S]()(u)
//    def value_=(s: S): Control[Unit] = put[S](s)(u)
//  }
//
//  def state[R, S] = new Handler.Stateful[R, R, S] with State[S] {
//    def unit = a => a
//    def put(s: S) = state => resume => resume(())(s)
//    def get() = state => resume => resume(state)(state)
//  }
//}
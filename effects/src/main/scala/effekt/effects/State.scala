package effekt
package effects

trait Reader[S] extends Eff {
  def ask(): Op[S]
}
trait ReaderSyntax {
  def ask[S]()(implicit u: Use[Reader[S]]): Control[S] =
    use(u)(u.effect.ask())
}
object Reader extends ReaderSyntax

trait Writer[S] extends Eff {
  def tell(s: S): Op[Unit]
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
 *   handle(state[Int]) { implicit s => prog }.runState(0)
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
    use(u)(u.effect.get())

  def put[S](s: S)(implicit u: Use[State[S]]): Control[Unit] =
    use(u)(u.effect.put(s))

  implicit class StateOps[S](val u: Use[State[S]]) extends AnyVal {
    def value: Control[S] = get[S]()(u)
    def value_=(s: S): Control[Unit] = put[S](s)(u)
  }

  implicit class ImplicitStateOps[S, A](val ca: Control[S => Control[A]]) extends AnyVal {
    def runState(s: S): Control[A] = ca.flatMap(f => f(s))
  }

  trait StateImpl[S, R] extends State[S] with Handler.Aux[R, S => Control[R]] {
    def unit = r => s => pure(r)

    def put(s: S) = resume => pure(_ => resume(()).runState(s))
    def get() = resume => pure(s => resume(s).runState(s))
  }

  def state[S, R](init: S)(
   f: Use[State[S]] => Control[R]
  ) = handle(new StateImpl[S, R] {})(f).runState(init)
}
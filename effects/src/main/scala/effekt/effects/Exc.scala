package effekt
package effects

trait Exc extends Eff {
  def raise[A](msg: String): Op[A]
}
object Exc {

  def raise[A](msg: String)(implicit u: Use[Exc]): Control[A] =
    use(u)(u.handler.raise[A](msg))

  def excOption[R] = new Handler.Basic[R, Option[R]] with Exc {
    def unit = a => Some(a)
    def raise[A](msg: String) = _ => resume => pure(None)
  }

  def fromException[R](filter: Exception => Boolean = _ => true) =
    new Handler.Basic[R, Either[Exception, R]] with Exc {
      def unit = a => Right(a)
      def raise[A](msg: String) = _ => resume => pure(Left(new Exception(msg)))
      override def _catch = {
        case e: Exception if filter(e) => pure(Left(e))
      }
    }

  def toException[R](c: Either[Exception, R]): R = c match {
    case Left(e) => throw e
    case Right(r) => r
  }
}
package effekt
package effects

trait Exc {
  def raise[A](msg: String): Control[A]
}
object Exc {

  def excOption[R] = new Handler[R, Option[R]] with Exc {
    def unit = a => pure(Some(a))
    def raise[A](msg: String) = use { resume => pure(None) }
  }

  def fromException[R](filter: Exception => Boolean = _ => true) =
    new Handler[R, Either[Exception, R]] with Exc {
      def unit = a => pure(Right(a))
      def raise[A](msg: String) = use { resume => pure(Left(new Exception(msg))) }
      override def _catch = {
        case e: Exception if filter(e) => pure(Left(e))
      }
    }

  def toException[R](c: Either[Exception, R]): R = c match {
    case Left(e) => throw e
    case Right(r) => r
  }
}
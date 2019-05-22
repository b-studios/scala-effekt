package effekt
package effects

trait Exc {
  def raise[A](msg: String): Control[A]
}
object Exc {

  def excOption[R](prog: Exc => Control[R]): Control[Option[R]] =
    new Handler[Option[R]] with Exc {
      def raise[A](msg: String) = use { resume => pure(None) }
    } handle { exc => prog(exc) map { r => Some(r) } }

  def fromException[R](filter: Exception => Boolean = _ => true)(prog: Exc => Control[R]) =
    new Handler[Either[Exception, R]] with Exc {
      def raise[A](msg: String) = use { resume => pure(Left(new Exception(msg))) }
    } handle { exc => prog(exc) map { r => Right(r) } } _catch {
      case e: Exception if filter(e) => pure(Left(e))
    }

  def toException[R](c: Either[Exception, R]): R = c match {
    case Left(e) => throw e
    case Right(r) => r
  }
}
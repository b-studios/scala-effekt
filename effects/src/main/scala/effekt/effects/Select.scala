package effekt
package effects

trait Select[A] {
  def select(as: List[A]): Control[A]
}
object Select {

  class SelectOption[R, A] extends Select[A] with Handler[Option[R]] {
    def select(as: List[A]) = use { resume => {
      def tryFirst(as: List[A]): Control[Option[R]] =
        if (as.isEmpty) {
          pure(None)
        } else resume(as.head) flatMap {
          case s @ Some(y) => pure(s)
          case None        => tryFirst(as.tail)
        }
      tryFirst(as)
    }}
  }

  def selectOption[R, A](prog: Select[A] => Control[R]): Control[Option[R]] =
    new SelectOption[R, A] handle { select => prog(select) map { r => Some(r) } }

  import cats._
  class SelectAlternative[F[_]: Alternative, R, A] extends Handler[F[R]] with Select[A] {
    def select(as: List[A]) = use { resume =>
      as.map { a => resume(a) }.foldLeft(pure(Alternative[F].empty[R])) {
        case (ys1, ys2) => for {
          y1 <- ys1
          y2 <- ys2
        } yield Alternative[F].combineK(y1, y2)
      }
    }
  }

  def selectAlternative[A, R, F[_]: Alternative](prog: Select[A] => Control[R]): Control[F[R]] =
    new SelectAlternative[F, R, A] handle { select => prog(select) map { Alternative[F].pure } }
}
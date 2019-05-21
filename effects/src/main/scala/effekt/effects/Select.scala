package effekt
package effects

trait Select[A] {
  def select(as: List[A]): Control[A]
}
object Select {

  class SelectOption[R, A] extends Select[A] with Handler[R, Option[R]] {

    def unit = a => pure(Some(a))

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

  import cats._
  def selectAlternative[A, R, F[_]: Alternative] = new Handler[R, F[R]] with Select[A] {

    val altF = Alternative[F]

    def unit = a => pure(altF.pure(a))

    def select(as: List[A]) = use { resume =>
      as.map { a => resume(a) }.foldLeft(pure(altF.empty[R])) {
        case (ys1, ys2) => for {
          y1 <- ys1
          y2 <- ys2
        } yield altF.combineK(y1, y2)
      }
    }
  }
}
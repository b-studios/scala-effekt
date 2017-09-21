package effekt
package effects

trait Select[A] extends Eff {
  def select(as: List[A]): Op[A]
}
object Select {

  def select[A](as: List[A])(implicit u: Use[Select[A]]): Control[A] =
    use(u)(u.effect.select(as))

  class SelectOption[R, A] extends Select[A] with Handler.Basic[R, Option[R]] {

    def unit = a => Some(a)

    def select(as: List[A]) = _ => resume => {
      def tryFirst(as: List[A]): Control[Option[R]] =
        if (as.isEmpty) {
          pure(None)
        } else resume(as.head)(()) flatMap {
          case s @ Some(y) => pure(s)
          case None        => tryFirst(as.tail)
        }
      tryFirst(as)
    }
  }

  import cats._
  def selectAlternative[A, R, F[_]: Alternative] = new Handler.Basic[R, F[R]] with Select[A] {

    val altF = Alternative[F]

    def unit = a => altF pure a

    def select(as: List[A]) = _ => resume => {
      as.map { a => resume(a)(()) }.foldLeft(pure(altF.empty[R])) {
        case (ys1, ys2) => for {
          y1 <- ys1
          y2 <- ys2
        } yield altF.combineK(y1, y2)
      }
    }
  }
}
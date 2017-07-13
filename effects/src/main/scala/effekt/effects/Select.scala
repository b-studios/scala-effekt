//package effekt
//package effects
//
//trait Select[A] extends Eff {
//  def select[R](as: List[A]): A @@ R
//}
//object Select {
//
//  def select[A](as: List[A])(implicit u: Use[Select[A]]): Control[A] =
//    use(u)(u.effect.select(as))
//
//  def selectOption[A] = new Select[A] {
//    type Out[A] = Option[A]
//    def unit[A] = a => Some(a)
//
//    def select[R](as: List[A]) = resume => {
//      def tryFirst(as: List[A]): Control[Option[R]] =
//        if (as.isEmpty) {
//          pure(None)
//        } else resume(as.head) flatMap {
//          case s @ Some(y) => pure(s)
//          case None        => tryFirst(as.tail)
//        }
//      tryFirst(as)
//    }
//  }
//
//  import cats._
//  def selectAlternative[A, F[_]: Alternative] = new Select[A] {
//
//    val altF = Alternative[F]
//    type Out[A] = F[A]
//    def unit[A] = a => altF pure a
//
//    def select[R](as: List[A]) = resume => {
//      as.map { a => resume(a) }.foldLeft(pure(altF.empty[R])) {
//        case (ys1, ys2) => for {
//          y1 <- ys1
//          y2 <- ys2
//        } yield altF.combineK(y1, y2)
//      }
//    }
//  }
//}
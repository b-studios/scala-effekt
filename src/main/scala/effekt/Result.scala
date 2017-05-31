package effekt

private[effekt]
sealed trait Result[+A]

private[effekt]
case class Pure[A](value: A) extends Result[A]

private[effekt]
case class Impure[A, R](c: Control[R], k: MetaCont[R, A]) extends Result[A]

private[effekt]
object Result {
  @scala.annotation.tailrec
  def trampoline[A](y: Result[A]): A = y match {
    case Pure(a) => a
    case Impure(c, k) => trampoline[A](c(k))
  }
}
package effekt

private[effekt]
sealed trait Result[+A]

private[effekt]
case class Value[+A](value: A) extends Result[A]

private[effekt]
case class Computation[+A, R](c: Control[R, _], k: MetaCont[R, A]) extends Result[A]

private[effekt]
case class Abort(t: Throwable) extends Result[Nothing]

private[effekt]
object Result {
  @scala.annotation.tailrec
  def trampoline[A](y: Result[A]): A = y match {
    case Value(a) => a
    case Computation(c, k) => trampoline[A](c(k))
    case Abort(t) => throw t
  }
}
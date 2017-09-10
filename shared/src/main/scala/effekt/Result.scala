package effekt

private[effekt]
sealed trait Result[+A] {
  def isPure: Boolean
}

private[effekt]
case class Pure[A](value: A) extends Result[A] {
  val isPure = true
}

private[effekt]
case class Impure[A, R](c: Control[R], k: MetaCont[R, A]) extends Result[A] {
  val isPure = false
}

private[effekt]
object Result {

  def trampoline[A](y: Result[A]): A = {
    var res: Result[A] = y

    while (!res.isPure) {
      val imp = res.asInstanceOf[Impure[A, Any]]
      try { res = imp.c(imp.k) } catch {
        case e: Throwable => imp.k.unwind(); throw e
      }
    }
    res.asInstanceOf[Pure[A]].value
  }
}
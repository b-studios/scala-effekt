package effekt

private[effekt]
sealed trait Result[+A] {
  def isComputation: Boolean
}

private[effekt]
case class Value[A](value: A) extends Result[A] {
  def isComputation: Boolean = false
}

private[effekt]
case class Computation[A, R](c: Control[R], k: MetaCont[R, A]) extends Result[A] {
  def isComputation: Boolean = true
}

private[effekt]
case class Abort(t: Throwable) extends Result[Nothing] {
  def isComputation: Boolean = false
}


private[effekt]
object Result {
  def trampoline[A](y: Result[A]): A = {
    var res: Result[A] = y

    while (res.isComputation) {
      val comp = res.asInstanceOf[Computation[A, Any]]

      res = try { comp.c(comp.k) } catch {
        case t: Throwable => comp.k.unwind(t)
      }
    }

    res match {
      case Value(a) => a
      case Abort(t) => throw t
      case Computation(c, k) => ???
    }
  }
}

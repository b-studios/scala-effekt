package effekt

private[effekt]
sealed trait MetaCont[-A, +B] extends Serializable {
  def apply(a: A): Result[B]

  def append[C](s: MetaCont[B, C]): MetaCont[A, C]

  def splitAt(c: Capability): (MetaCont[A, c.Res], H[c.type], MetaCont[c.Res, B])

  def map[C](f: C => A): MetaCont[C, B] = flatMap(f andThen pure) //PureCont(f, this)

  def flatMap[C](f: Frame[C, A]): MetaCont[C, B] = FrameCont(f, this)
}

private[effekt]
case class ReturnCont[-A, +B](f: A => B) extends MetaCont[A, B] {
  final def apply(a: A): Result[B] = Pure(f(a))

  final def append[C](s: MetaCont[B, C]): MetaCont[A, C] = s map f

  final def splitAt(c: Capability) = sys error s"Prompt $c not found on the stack."

  override def map[C](g: C => A): MetaCont[C, B] = ReturnCont(g andThen f)
}

private[effekt]
case class CastCont[-A, +B]() extends MetaCont[A, B] {
  final def apply(a: A): Result[B] = Pure(a.asInstanceOf[B])

  final def append[C](s: MetaCont[B, C]): MetaCont[A, C] = s.asInstanceOf[MetaCont[A, C]]

  final def splitAt(c: Capability) = sys error s"Prompt $c not found on the stack."

  override def map[C](g: C => A): MetaCont[C, B] = ReturnCont(x => g(x).asInstanceOf[B])
}

private[effekt]
case class FrameCont[-A, B, +C](frame: Frame[A, B], tail: MetaCont[B, C]) extends MetaCont[A, C] {
  final def apply(a: A): Result[C] = Impure(frame(a), tail)

  final def append[D](s: MetaCont[C, D]): MetaCont[A, D] = (tail append s) flatMap frame

  final def splitAt(c: Capability) = tail.splitAt(c) match {
    case (head, matched, tail) => (head flatMap frame, matched, tail)
  }

  override def map[D](g: D => A): MetaCont[D, C] = tail flatMap (g andThen frame)

  override def flatMap[D](f: Frame[D, A]): MetaCont[D, C] = tail flatMap { x => f(x).flatMap(frame) }

}

private[effekt]
case class HandlerCont[R, A](h: Handler {type Res = R}, tail: MetaCont[R, A]) extends MetaCont[R, A] {
  final def apply(r: R): Result[A] = tail(r)

  final def append[C](s: MetaCont[A, C]): MetaCont[R, C] = HandlerCont(h, tail append s)

  final def splitAt(c: Capability) =
  // Here we deduce type equality from referential equality
    if (h.prompt eq c) {
      val head = CastCont[R, c.Res]()
      val handler = h.asInstanceOf[H[c.type]]
      val rest = tail.asInstanceOf[MetaCont[c.Res, A]]
      (head, handler, rest)
    } else tail.splitAt(c) match {
      case (head, matched, tail) => (HandlerCont(h, head), matched, tail)
    }
}

package effekt

private[effekt]
sealed trait MetaCont[-A, +B] extends Serializable {
  def apply(a: A): Result[B]

  def append[C](s: MetaCont[B, C]): MetaCont[A, C]

  def splitAt[Res](c: Prompt[Res]): (MetaCont[A, Res], MetaCont[Res, B])

  def map[C](f: C => A): MetaCont[C, B] = flatMap(x => pure(f(x)))

  def flatMap[C](f: Frame[C, A]): MetaCont[C, B] = FramesCont(List(f), this)
}

private[effekt]
case class ReturnCont[A]() extends MetaCont[A, A] {
  final def apply(a: A): Result[A] = Pure(a)

  final def append[B](s: MetaCont[A, B]): MetaCont[A, B] = s

  final def splitAt[Res](c: Prompt[Res]) = sys error s"Prompt $c not found on the stack."
}

private[effekt]
case class FramesCont[-A, B, +C](frames: List[Frame[Nothing, Any]], tail: MetaCont[B, C]) extends MetaCont[A, C] {

  final def apply(a: A): Result[C] = {
    val first :: rest = frames
    val result = first.asInstanceOf[Frame[A, B]](a)
    if (rest.isEmpty) {
      Impure(result, tail)
    } else {
      Impure(result, FramesCont(rest, tail))
    }
  }

  final def append[D](s: MetaCont[C, D]): MetaCont[A, D] = FramesCont(frames, tail append s)

  final def splitAt[Res](c: Prompt[Res]) = tail.splitAt(c) match {
    case (head, tail) => (FramesCont(frames, head), tail)
  }

  override def flatMap[D](f: Frame[D, A]): MetaCont[D, C] = FramesCont(f :: frames, tail)
}

private[effekt]
case class HandlerCont[Res, +A](h: Prompt[Res])(tail: MetaCont[Res, A]) extends MetaCont[Res, A] {
  final def apply(r: Res): Result[A] = tail(r)

  final def append[C](s: MetaCont[A, C]): MetaCont[Res, C] = HandlerCont(h)(tail append s)

  final def splitAt[Res2](c: Prompt[Res2]) = c match {
    // Here we deduce type equality from referential equality
    case _: h.type => (HandlerCont(h)(ReturnCont()), tail)
    case _ => tail.splitAt(c) match {
      case (head, tail) => (HandlerCont(h)(head), tail)
    }
  }
}

// we will NEVER split at a state cont. Instead a statecont
// just calls into the Stateful interface on capture
private[effekt]
case class StateCont[Res, S, +A](h: Stateful[S], state: S, tail: MetaCont[Res, A]) extends MetaCont[Res, A] {
  final def apply(r: Res): Result[A] = tail(r)

  final def append[C](rest: MetaCont[A, C]): MetaCont[Res, C] = {
    h put state
    StateCont(h, state, tail append rest)
  }

  final def splitAt[Res2](c: Prompt[Res2]) = tail.splitAt(c) match {
    case (head, tail) => (StateCont(h, h.get(), head), tail)
  }
}
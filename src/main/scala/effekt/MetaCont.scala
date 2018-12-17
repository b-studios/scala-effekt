package effekt

private[effekt]
sealed trait MetaCont[-A, +B] extends Serializable {
  def apply(a: A): Result[B]

  def append[C](s: MetaCont[B, C]): MetaCont[A, C]

  def splitAt(c: Prompt): (MetaCont[A, c.Result], MetaCont[c.Result, B])

  def map[C](f: C => A): MetaCont[C, B] = flatMap(x => pure(f(x)))

  def flatMap[C](f: Frame[C, A]): MetaCont[C, B] = FramesCont(List(f), this)
}

private[effekt]
case class ReturnCont[A]() extends MetaCont[A, A] {
  final def apply(a: A): Result[A] = Value(a)
  final def append[B](s: MetaCont[A, B]): MetaCont[A, B] = s
  final def splitAt(c: Prompt) = sys error s"Prompt $c not found on the stack."
}

private[effekt]
case class FramesCont[-A, B, +C](frames: List[Frame[Nothing, Any]], tail: MetaCont[B, C]) extends MetaCont[A, C] {

  final def apply(a: A): Result[C] = {
    val first :: rest = frames
    val result = first.asInstanceOf[Frame[A, B]](a)
    if (rest.isEmpty) {
      Computation(result, tail)
    } else {
      Computation(result, FramesCont(rest, tail))
    }
  }

  final def append[D](s: MetaCont[C, D]): MetaCont[A, D] = FramesCont(frames, tail append s)

  final def splitAt(c: Prompt) = tail.splitAt(c) match {
    case (head, tail) => (FramesCont(frames, head), tail)
  }

  override def flatMap[D](f: Frame[D, A]): MetaCont[D, C] = FramesCont(f :: frames, tail)
}

private[effekt]
case class PromptCont[Res, +A](p: Prompt { type Result = Res })(tail: MetaCont[Res, A]) extends MetaCont[Res, A] {
  final def apply(r: Res): Result[A] = tail(r)

  final def append[C](s: MetaCont[A, C]): MetaCont[Res, C] = PromptCont(p)(tail append s)

  // Here we can see that our semantics is closer to spawn/controller than delimCC
  final def splitAt(c: Prompt) = c match {
    // Here we deduce type equality from referential equality
    case _: p.type =>
      val head = PromptCont(p)(ReturnCont[p.Result]()).asInstanceOf[MetaCont[Res, c.Result]]
      val rest = tail.asInstanceOf[MetaCont[c.Result, A]]
      (head, rest)

    case _ => tail.splitAt(c) match {
      case (head, tail) =>
        (PromptCont(p)(head), tail)
    }
  }
}

private[effekt]
case class StateCont[Res, +A](p: State, tail: MetaCont[Res, A]) extends MetaCont[Res, A] {
  final def apply(r: Res): Result[A] = tail(r)

  final def append[C](k: MetaCont[A, C]): MetaCont[Res, C] = StateCont(p, tail append k)

  final def splitAt(c: Prompt) = tail.splitAt(c) match {
    case (head, tail) => (StateContCaptured(p, head)(p.backup), tail)
  }
}

private[effekt]
case class StateContCaptured[Res, +A](p: State, tail: MetaCont[Res, A])(state: p.StateRep) extends MetaCont[Res, A] {
  final def apply(r: Res): Result[A] = ???
  final def append[C](s: MetaCont[A, C]): MetaCont[Res, C] = StateCont({ p restore state; p }, tail append s)
  final def splitAt(c: Prompt) = ???
}
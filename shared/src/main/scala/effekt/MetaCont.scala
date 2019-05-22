package effekt

private[effekt]
sealed trait MetaCont[-A, +B] extends Serializable {
  def apply(a: A): Result[B]

  def append[C](s: MetaCont[B, C]): MetaCont[A, C]

  def splitAt[Res](c: ContMarker[Res]): (MetaCont[A, Res], MetaCont[Res, B])

  def map[C](f: C => A): MetaCont[C, B] = flatMap(x => pure(f(x)))

  def flatMap[C](f: Frame[C, A]): MetaCont[C, B] = FramesCont(List(f), this)

  def unwind(t: Throwable): Result[B]
}

private[effekt]
case class ReturnCont[A]() extends MetaCont[A, A] {
  final def apply(a: A): Result[A] = Value(a)

  final def append[B](s: MetaCont[A, B]): MetaCont[A, B] = s

  final def splitAt[Res](c: ContMarker[Res]) = sys error s"Prompt $c not found on the stack."

  final def unwind(t: Throwable) = throw t

  override def toString = "[]"
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

  final def splitAt[Res](c: ContMarker[Res]) = tail.splitAt(c) match {
    case (head, tail) => (FramesCont(frames, head), tail)
  }

  override def flatMap[D](f: Frame[D, A]): MetaCont[D, C] = FramesCont(f :: frames, tail)

  final def unwind(t: Throwable) = tail.unwind(t)

  override def toString = s"fs :: ${tail}"
}

private[effekt]
case class HandlerCont[Res, +A](marker: ContMarker[Res])(tail: MetaCont[Res, A]) extends MetaCont[Res, A] {
  final def apply(r: Res): Result[A] = tail(r)

  final def append[C](s: MetaCont[A, C]): MetaCont[Res, C] = HandlerCont(marker)(tail append s)

  final def splitAt[Res2](c: ContMarker[Res2]) = c match {
    // Here we deduce type equality from referential equality
    case _: marker.type => (HandlerCont(marker)(ReturnCont()), tail)
    case _ => tail.splitAt(c) match {
      case (head, tail) => (HandlerCont(marker)(head), tail)
    }
  }

  final def unwind(t: Throwable) = tail.unwind(t)

  override def toString = s"prompt ${marker} :: ${tail}"
}

private[effekt]
case class StateCont[Res, +A](marker: StateMarker, tail: MetaCont[Res, A]) extends MetaCont[Res, A] {
  final def apply(r: Res): Result[A] = tail(r)

  final def append[C](k: MetaCont[A, C]): MetaCont[Res, C] = StateCont(marker, tail append k)

  final def splitAt[Res2](c: ContMarker[Res2]) = tail.splitAt(c) match {
    case (head, tail) => (StateContCaptured(marker, marker.backup, head), tail)
  }

  final def unwind(t: Throwable) = tail.unwind(t)

  override def toString = s"state ${marker} :: ${tail}"
}

private[effekt]
case class CatchCont[Res, +A](marker: CatchMarker[Res], tail: MetaCont[Res, A]) extends MetaCont[Res, A] {
  final def apply(r: Res): Result[A] = tail(r)

  final def append[C](k: MetaCont[A, C]): MetaCont[Res, C] = CatchCont(marker, tail append k)

  final def splitAt[Res2](c: ContMarker[Res2]) = tail.splitAt(c) match {
    case (head, tail) => (CatchCont(marker, head), tail)
  }

  final def unwind(t: Throwable) =
    if (marker._catch.isDefinedAt(t)) {
      marker._catch(t)(tail)
    } else {
      tail.unwind(t)
    }

  override def toString = s"catch ${marker} :: ${tail}"
}

private[effekt]
case class StateContCaptured[Res, S, +A](p: StateMarker { type StateRep = S }, state: S, tail: MetaCont[Res, A]) extends MetaCont[Res, A] {
  final def append[C](s: MetaCont[A, C]): MetaCont[Res, C] = StateCont({ p restore state ; p }, tail append s)

  // these should not be called.
  final def apply(r: Res): Result[A] = ???
  final def splitAt[Res2](c: ContMarker[Res2]) = ???
  final def unwind(t: Throwable) = ???
}
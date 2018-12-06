package effekt

private[effekt]
sealed trait MetaCont[-A, +B] extends Serializable {
  def apply(a: A): Result[B]

  def append[C](s: MetaCont[B, C]): MetaCont[A, C]

  def splitAt(c: Prompt): (MetaCont[A, c.Result], MetaCont[c.Result, B])

  def map[C](f: C => A): MetaCont[C, B] = flatMap(x => pure(f(x)))

  def flatMap[C](f: Frame[C, A]): MetaCont[C, B] = FramesCont(List(f), this)

  def bind(key: Key): MetaCont[A, B] = StateCont(Set(key), Set.empty, this)
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
        (PromptStateCont(p)(p.backup, head), tail)
    }
  }
}

private[effekt]
case class PromptStateCont[Res, +A](p: Prompt { type Result = Res })(state: p.State, tail: MetaCont[Res, A]) extends MetaCont[Res, A] {
  final def apply(r: Res): Result[A] = ???
  final def append[C](s: MetaCont[A, C]): MetaCont[Res, C] =
    PromptCont({ p restore state; p })(tail append s)
  final def splitAt(c: Prompt) = ???
}

private[effekt]
case class StateCont[-A, +B](keys: Set[Key], bindings: Set[(Key, Any)], tail: MetaCont[A, B]) extends MetaCont[A, B] {
  final def apply(a: A): Result[B] = tail(a)
  final def append[C](s: MetaCont[B, C]): MetaCont[A, C] =
    StateCont(keys, { restore(bindings); Set.empty }, tail append s)
  final def splitAt(c: Prompt) = tail.splitAt(c) match {
    case (head, tail) => (StateCont(keys, backup(keys), head), tail)
  }

  // the shadowed binding is unreachable, so we
  // avoid unnecessary spilling of the heap with shadowed frames at
  // the runtime cost of rebinding
  //
  // Also: Stateconts can be commuted if they don't shadow each other
  final override def bind(key: Key): MetaCont[A, B] = StateCont(keys + key, bindings, tail)

  def backup(keys: Set[Key]): Set[(Key, Any)] = keys.map { k => (k, k.get) }
  def restore(bindings: Set[(Key, Any)]): Unit = bindings.foreach { (k, v) => k.set(v.asInstanceOf[Nothing]) }
}

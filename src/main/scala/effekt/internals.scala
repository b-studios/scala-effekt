package effekt

package object internals {

  private[effekt]
  case class Pure[A](a: A) extends I[A] {
    def run = a
    override def map[B](f: A => B): I[B] = pure(f(a))
    override def map2[B, C](mb: I[B])(f: (A, B) => C): I[C] = mb.map { b => f(a, b) }
    def flatMap[B](f: A => C[B]): C[B] = f(a)
  }


    // The stack is assumed to have the shape:
    //
    // |       |             |
    // +-------+-------------|
    // |  op() |      k1     |  idiomatic fragment of the stack (UseI)  \
    // |  ...  | map2(...)   |                                          |
    // +-------+-------------+                                           > UseD
    // |      flatMap(f1)    |                                          |
    // |      flatMap(f2)    |  monadic fragment of the stack   (UseM)  /
    // |        ...          |
    // +---------------------+
    // |    handleDynamic()  |  the handler (dynamic handler with access to both fragments)

  private[effekt]
  case class UseI[A, X](h: handler.Idiomatic, body: h.CPS[X], ki: I[X => A]) extends I[A] {
    def run = sys error "undelimited idiom"
    def map[B](f: A => B): I[B] =
      UseI(h, body, ki map { _ andThen f })
    def map2[B, C](mb: I[B])(f: (A, B) => C): I[C] =
      UseI(h, body, ki.map2(mb) { (xa, b) => x => f(xa(x), b) })

    // Here we need to idiomatically reset the idiomatic continuation...
    // This is similar to installing a separate, idiomatic handler that
    // communicates with the outer (monadic) handler via `run`.
    def flatMap[B](f: A => C[B]): C[B] = {
      // a flatMap acts like a reset for the inner (idiomatic) handler
      // now it is up to the outer (monadic) handler
      UseD(h, h.CPS[X, A](body)(reset(h) { ki }), f)
    }
  }

  private[effekt]
  case class UseD[A, X](h: handler.Idiomatic, ki: I[h.G[X]], km: X => C[A]) extends C[A] {
    def run = sys error "undelimited idiom"
    def map[B](f: A => B): C[B] = UseD(h, ki, a => km(a) map f)
    def flatMap[B](f: A => C[B]): C[B] = UseD(h, ki, a => km(a) flatMap f)
  }

  // TODO here UseM extends I since the effect signatures promise idiomatic effects
  // It should be C[A] after the first call to flatMap! Otherwise the signature of `map` is a lie.
  // This could be addressed by yet another type of bubble UseMI
  private[effekt]
  case class UseM[A, X](h: handler.Monadic[_], body: h.CPS[X], km: X => C[A]) extends I[A] {
    def run = sys error "undelimited control"
    def map[B](f: A => B): I[B] = UseM(h, body, a => km(a) map f)
    def flatMap[B](f: A => C[B]): C[B] = UseM(h, body, a => km(a) flatMap f)

    // TODO is this correctly mixing idiomatic and monadic?
    def map2[B, D](mb: I[B])(f: (A, B) => D): I[D] =
      UseM(h, body, a => km(a).map2(mb)(f))
  }

  private[effekt]
  def reset[R](hi: handler.Idiomatic)(prog: I[R]): I[hi.G[R]] = prog match {
    case p @ Pure(_) =>
      p.map(hi.unit)

    // since h eq u.h
    // we can assume that h.G =:= u.h.G
    case u : UseI[R, _] { val h: hi.type } if hi eq u.h =>
      u.h.CPS(u.body)(reset(u.h) { u.ki })

    case u : UseI[R, x] =>
      val k: I[x => hi.G[R]] = reset(hi)(u.ki) map { gk => x =>
        // here we require G to be a functor to convert `gk: G[x => R]` to `x => G[R]`:
        hi.map[x => R, R](xr => xr(x))(gk)
      }
      UseI(u.h, u.body, k)

    // interaction between outer monadic handlers and idiomatic programs:
    //   handleMonad { m => handleIdiom { i => idiomProg(... m.op() ...) }}
    case u : UseM[a, x] =>
      // FIXME doesn't typecheck, since the continuation is not idiomatic. So we can't reset it
      //       with this reset. Hence the cast.
      // this case is only necessary since we declared UseM <: I
      // we could have a separate bubble type for UseMI that is an idiomatic prog.
      // This way we could avoid the cast.
      UseM(u.h, u.body, x => reset(hi) { u.km(x).asInstanceOf[I[R]] })

    case u : UseD[R, x] =>
      sys error "Should not occur. Unhandled idiomatic computation " + u
  }

  // lowers an idiomatic handler to a monadic handler
  private[effekt]
  def dynamic[R](hi: handler.Idiomatic, run: hi.G[ω] => (ω => C[R]) => C[R])(prog: hi.type => C[R]): C[R] = prog(hi) match {
    case p : Pure[R] => p

    case u : UseD[R, ω] { val h: hi.type } if hi eq u.h =>
      u.ki flatMap { go => run(go) { x => dynamic(hi, run) { _ => u.km(x) }}}

    case u : UseD[R, x] =>
      UseD(u.h, u.ki, x => dynamic(hi, run) { _ => u.km(x) })

    case u : UseM[R, x] =>
      UseM(u.h, u.body, x => dynamic(hi, run) { _ => u.km(x) })

    // the program is purely idiomatic, no flatMap occurred.
    case u : UseI[R, x] if hi eq u.h =>
      dynamic(hi, run)(_ => u flatMap { pure })

    // Like with `reset(Monadic)`, in presence of `dynamic` this
    // we need to force a conversion from UseI to UseD here.
    case u : UseI[R, x] =>
      dynamic(hi, run)(_ => u flatMap { pure })
  }


  private[effekt]
  def reset[R](hm: handler.Monadic[R])(prog: C[R]): C[R] = prog match {
    case p: Pure[R] => p

    case u : UseM[R, _] { val h: hm.type } if hm eq u.h =>
      u.body(x => reset(hm) { u.km(x) })

    case u : UseM[a, x] =>
      UseM(u.h, u.body, x => reset(hm) { u.km(x) })

    case u: UseD[a, x] =>
      UseD(u.h, u.ki, x => reset(hm) { u.km(x) })

    // Can only occur through handleIdiom { i => handleMonadic { m => i.op() } }
    // but this should be ruled out by `handleMonadic: C[R]` and `handleIdiom(I[R])`.
    //
    // No: In presence of `dynamic` this is in deed possible, so we need
    // to force a conversion from UseI to UseD here.
    case i : UseI[r, x] =>
      reset(hm) { prog flatMap pure }
  }
}
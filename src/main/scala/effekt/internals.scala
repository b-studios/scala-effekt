package effekt

import handler._

package object internals {

  case class EffOp[H <: Handler, X](h: H, body: h.CPS[X]) {
    def apply[R](k: h.Cont[X, R]): h.Answer[R] = h.CPS(body)(k)
  }

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
  case class UseI[A, X](op: EffOp[Idiomatic, X], ki: I[X => A]) extends I[A] {
    def run = sys error "undelimited idiom"
    def map[B](f: A => B): I[B] =
      copy(ki = ki map { _ andThen f })
    def map2[B, C](mb: I[B])(f: (A, B) => C): I[C] =
      copy(ki = ki.map2(mb) { (xa, b) => x => f(xa(x), b) })

    // Here we need to idiomatically reset the idiomatic continuation...
    // This is similar to installing a separate, idiomatic handler that
    // communicates with the outer (monadic) handler via `run`.
    def flatMap[B](f: A => C[B]): C[B] =
      UseD(op, ki, f)
  }

  private[effekt]
  case class UseD[A, X, Y](op: EffOp[Idiomatic, X], ki: I[X => Y], km: Y => C[A]) extends C[A] {
    def run = sys error "undelimited idiom"
    def map[B](f: A => B): C[B] = copy(km = a => km(a) map f)
    def flatMap[B](f: A => C[B]): C[B] = copy(km = a => km(a) flatMap f)
  }

  // TODO here UseM extends I since the effect signatures promise idiomatic effects
  // It should be C[A] after the first call to flatMap! Otherwise the signature of `map` is a lie.
  // This could be addressed by yet another type of bubble UseMI
  private[effekt]
  case class UseM[A, X](op: EffOp[Monadic, X], km: X => C[A]) extends I[A] {
    def run = sys error "undelimited control"
    def map[B](f: A => B): I[B] = copy(km = a => km(a) map f)
    def flatMap[B](f: A => C[B]): C[B] = copy(km = a => km(a) flatMap f)
    def map2[B, D](mb: I[B])(f: (A, B) => D): I[D] = copy(km = a => km(a).map2(mb)(f))
  }

  private[effekt]
  def reset[R](hi: Idiomatic)(prog: I[R]): I[hi.G[R]] = prog match {
    case p @ Pure(_) =>
      p.map(hi.unit)

    // since h eq u.h
    // we can assume that h.G =:= u.h.G
    case u : UseI[R, ω] { val op: EffOp[hi.type, ω] } if hi eq u.op.h =>
      u.op[R] { reset(hi) { u.ki } }

    case u : UseI[R, x] =>
      val k: I[x => hi.G[R]] = reset(hi)(u.ki) map { gk => x =>
        // here we require G to be a functor to convert `gk: G[x => R]` to `x => G[R]`:
        hi.map[x => R, R](xr => xr(x))(gk)
      }
      u.copy(ki = k)

    // interaction between outer monadic handlers and idiomatic programs:
    //   handleMonad { m => handleIdiom { i => idiomProg(... m.op() ...) }}
    case u : UseM[a, x] =>
      // FIXME doesn't typecheck, since the continuation is not idiomatic. So we can't reset it
      //       with this reset. Hence the cast.
      // this case is only necessary since we declared UseM <: I
      // we could have a separate bubble type for UseMI that is an idiomatic prog.
      // This way we could avoid the cast.
      u.copy(km = x => reset(hi) { u.km(x).asInstanceOf[I[R]] })

    case u : UseD[R, x, y] =>
      sys error "Should not occur. Unhandled idiomatic computation " + u
  }

  // lowers an idiomatic handler to a monadic handler
  private[effekt]
  def dynamic[R](hi: Idiomatic, run: hi.G[ω] => (ω => C[R]) => C[R])(prog: hi.type => C[R]): C[R] = prog(hi) match {
    case p : Pure[R] => p

    // Here is where the magic happens!
    // The first flatMap acts like a reset for the inner (idiomatic) handler
    case u : UseD[R, ω, ω] { val op: EffOp[hi.type, ω] } if hi eq u.op.h =>
      // 1) Reset the idiomatic continuation with the idiomatic handler
      // 2) Burst the idiomatic bubble
      // 2) Run the remaining computation with the resulting value (`g`).
      val ig: I[hi.G[ω]] = u.op { reset(hi) { u.ki } }
      ig flatMap { g => run(g) { x => dynamic(hi, run) { _ => u.km(x) } } }

    case u : UseD[R, x, ω] =>
      u.copy(km = x => dynamic(hi, run) { _ => u.km(x) })

    case u : UseM[R, x] =>
      u.copy(km = x => dynamic(hi, run) { _ => u.km(x) })

    // the program is purely idiomatic, no flatMap occurred.
    case u : UseI[R, x] if hi eq u.op.h =>
      dynamic(hi, run)(_ => u flatMap { pure })

    // Like with `reset(Monadic)`, in presence of `dynamic` this
    // we need to force a conversion from UseI to UseD here.
    case u : UseI[R, x] =>
      dynamic(hi, run)(_ => u flatMap { pure })
  }

  private[effekt]
  def reset[R](hm: Monadic)(prog: C[hm.G[R]]): C[hm.G[R]] = prog match {
    case p: Pure[_] => p

    case u : UseM[hm.G[ω], ω] { val op: EffOp[hm.type, ω] } if hm eq u.op.h =>
      u.op { x => reset(hm) { u.km(x) } }

    case u : UseM[a, x] =>
      u.copy(km = x => reset(hm) { u.km(x) })

    case u: UseD[a, x, y] =>
      u.copy(km = x => reset(hm) { u.km(x) })

    // Can only occur through handleIdiom { i => handleMonadic { m => i.op() } }
    // but this should be ruled out by `handleMonadic: C[R]` and `handleIdiom(I[R])`.
    //
    // No: In presence of `dynamic` this is in deed possible, so we need
    // to force a conversion from UseI to UseD here.
    case i : UseI[r, x] =>
      reset(hm) { prog flatMap pure }
  }
}
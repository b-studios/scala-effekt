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

  // A "Bubble" that collects an idiomatic context.
  private[effekt]
  case class UseI[X, A](op: EffOp[_, X], ki: I[X => A]) extends I[A] {
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
  case class UseD[X, Y, A](op: EffOp[_, X], ki: I[X => Y], km: Y => C[A]) extends C[A] {
    def run = sys error "undelimited idiom"
    def map[B](f: A => B): C[B] = copy(km = a => km(a) map f)
    def flatMap[B](f: A => C[B]): C[B] = copy(km = a => km(a) flatMap f)
  }


  private[effekt]
  def reset[R](hi: Idiomatic)(prog: I[R]): I[hi.G[R]] = prog match {
    case p @ Pure(_) =>
      p.map(hi.unit)

    // since h eq u.h
    // we can assume that h.G =:= u.h.G
    case u : UseI[τ, R] { val op: EffOp[hi.type, τ] } if hi eq u.op.h =>
      u.op[R] { reset(hi) { u.ki } }

    case u : UseI[x, R] =>
      val k: I[x => hi.G[R]] = reset(hi)(u.ki) map { gk => x =>
        // here we require G to be a functor to convert `gk: G[x => R]` to `x => G[R]`:
        hi.map[x => R, R](xr => xr(x))(gk)
      }
      u.copy(ki = k)

    case u : UseD[x, y, R] =>
      sys error "Should not occur. Unhandled idiomatic computation " + u
  }

  // lowers an idiomatic handler to a monadic handler
  private[effekt]
  def dynamic[R](hi: Idiomatic, run: hi.G[ω] => (ω => C[R]) => C[R])(prog: hi.type => C[R]): C[R] = prog(hi) match {
    case p : Pure[R] => p

    // Here is where the magic happens!
    // The first flatMap acts like a reset for the inner (idiomatic) handler
    case u : UseD[τ, ω, R] { val op: EffOp[hi.type, τ] } if hi eq u.op.h =>
      // 1) Reset the idiomatic continuation with the idiomatic handler
      // 2) Burst the idiomatic bubble
      // 2) Run the remaining computation with the resulting value (`g`).
      val ig: I[hi.G[ω]] = u.op { reset(hi) { u.ki } }
      val kn: ω => C[R]  = x => dynamic(hi, run) { _ => u.km(x) }
      ig flatMap { g => run(g)(kn) }

    case u : UseD[x, ω, R] =>
      u.copy(km = x => dynamic(hi, run) { _ => u.km(x) })

    // the program is purely idiomatic, no flatMap occurred.
    case u : UseI[x, R] if hi eq u.op.h =>
      dynamic(hi, run)(_ => u flatMap { pure })

    // Like with `reset(Monadic)`, in presence of `dynamic` this
    // we need to force a conversion from UseI to UseD here.
    case u : UseI[x, R] =>
      dynamic(hi, run)(_ => u flatMap { pure })
  }

  private[effekt]
  def reset[R](hm: Monadic)(prog: C[hm.G[R]]): C[hm.G[R]] = prog match {
    case p: Pure[_] => p

    // even though the handler is monadic, the continuation *can* have an
    // idiomatic fragment.
    case u : UseD[τ, ω, hm.G[R]] { val op: EffOp[hm.type, τ] } if hm eq u.op.h =>
      val ki: I[τ => ω]       = u.ki
      val km: ω => C[hm.G[R]] = u.km
      val kj: τ => I[ω]       = x => ki map { f => f(x) }
      val kn: τ => C[hm.G[R]] = x => kj(x) flatMap km
      u.op { x => reset(hm) { kn(x) } }

    case u: UseD[x, y, hm.G[R]] =>
      u.copy(km = x => reset(hm) { u.km(x) })

    // Can only occur through handleIdiom { i => handleMonadic { m => i.op() } }
    // but this should be ruled out by `handleMonadic: C[R]` and `handleIdiom(I[R])`.
    //
    // No: In presence of `dynamic` this is indeed possible, so we need
    // to force a conversion from UseI to UseD here.
    case i : UseI[x, hm.G[R]] =>
      reset(hm) { prog flatMap pure }
  }
}
package effekt
package handler

trait Idiomatic {
  type G[_]
  type CPS[A] = I[G[A => ω]] => I[G[ω]]

  // G is pointed (would be equiv to providing G[Unit])
  def unit[R]: R => G[R]

  // G needs to be a functor.
  def map[A, B]: (A => B) => G[A] => G[B]

  // ideally use would be parametric in R (with a rank-2 type).
  def useEff[A](body: CPS[A]): I[A] =
    internals.UseI(this, body, pure(a => a))

  // useEff is a bit more expressive, but this often suffices
  def use[A](body: G[A => ω] => G[ω]): I[A] =
    useEff { _ map body }

  def handle[R](prog: this.type => I[R]): I[G[R]] =
    effekt.handle(this)(prog)

  def apply[R](prog: this.type => I[R]): I[G[R]] =
    effekt.handle(this)(prog)

  // TODO can we somehow return a Monadic[R] here?
  def dynamic[R](prog: this.type => C[R])(run: G[ω] => (ω => C[R]) => C[R]): C[R] =
    effekt.dynamic(this, run)(prog)

  private[effekt] def CPS[A, R](body: CPS[A])(g: I[G[A => R]]): I[G[R]] =
    body.asInstanceOf[I[G[A => R]] => I[G[R]]](g)
}

// a handler for monadic programs that is itself monadic normal bubble semantics
trait Monadic[R] {
  type CPS[A] = (A => C[R]) => C[R]
  def use[A](body: CPS[A]): I[A] = internals.UseM(this, body, pure)

  def handle(prog: this.type => C[R]): C[R] = effekt.handle(this)(prog)
  def apply(prog: this.type => C[R]): C[R] = effekt.handle(this)(prog)
}


// Some other derived handlers

trait Id extends Idiomatic {
  type G[X] = X
  def unit[R] = identity
  def map[A, B] = _.apply

  // since G is Id, we can also run the handler on monadic computations
  def handleMonadic[R](prog: this.type => C[R]): C[R] =
    dynamic(prog) { res => resume => resume(res) }
}
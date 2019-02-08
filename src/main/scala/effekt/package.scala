package object effekt {

  // Used for a simplified encoding of rank-2 types.
  // It is not really safe, since values of type ω can
  // be stored in references and recovered later.
  type ω
  type Omega = ω

  type τ
  type Tau = τ

  type using[A, E] = implicit E => A
  type and[A, E] = implicit E => A

  type C[+A] = Control[A]
  type I[+A] = Idiom[A]

  def pure[A](value: => A): I[A] = new internals.Pure(value)
  def run[A](i: C[A]): A = i.run

  def handle[R](h: handler.Idiomatic)(prog: h.type => I[R]): I[h.G[R]] =
    internals.reset(h) { prog(h) }

  def handle[R](h: handler.Monadic)(prog: h.type => C[R]): C[h.G[R]] =
    internals.reset(h) { prog(h) map h.unit }

  // lowers an idiomatic handler to a monadic handler
  def dynamic[R](hi: handler.Idiomatic, run: hi.G[ω] => (ω => C[R]) => C[R])(prog: hi.type => C[R]): C[R] =
    internals.dynamic(hi, run)(prog)
}

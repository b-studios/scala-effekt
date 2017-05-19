package effekt

trait Eff extends Serializable {
  type Out[A]
  type State
  type @@[A, R] = State => (A => State => Control[Out[R]]) => Control[Out[R]]

  def unit[A]: (State, A) => Out[A]
}
package effekt

import scala.util.escape._

trait Eff extends Serializable {
  type Out[A]
  type State
  type @@[A, R] = State => (A => State => Control[Out[R]]) -> Control[Out[R]]

  def unit[A]: (State, A) => Out[A]

  protected[this] implicit def noState[A, R](f: (A => Control[Out[R]]) -> Control[Out[R]] ): A @@ R =
    s => resume => f(a => resume(a)(s))

  protected[this] implicit def just[A, R](a: A): A @@ R =
    s => resume => resume(a)(s)
}

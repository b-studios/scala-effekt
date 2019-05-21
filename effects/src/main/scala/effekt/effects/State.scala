package effekt
package effects

trait Reader[S] {
  def ask(): Control[S]
}

trait Writer[S] {
  def tell(s: S): Control[Unit]
}


/**
 * A simple state effect using builtin state.
 */
trait State[S] extends Reader[S] with Writer[S] {
  def put(s: S): Control[Unit]
  def get(): Control[S]
  def ask() = get()
  def tell(s: S) = put(s)
}
object State {

  def state[R, S](s0: S) = new Handler[R, R] with State[S] with effekt.State {
    val cell = Field(s0)
    def unit = a => pure(a)
    def put(s: S) = use { resume => for {
        _ <- cell.value = s
        r <- resume(())
      } yield r
    }
    def get() = use { resume => cell.value flatMap resume }
  }
}
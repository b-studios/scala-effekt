package effekt
package effects

trait Amb extends Eff {
  def flip[R](): Boolean @@ R
}
object Amb {
  def flip()(implicit u: Use[Amb]): Control[Boolean] =
    use(u)(u.effect.flip())

  object ambList extends Amb {
    type State  = Unit
    type Out[A] = List[A]
    def unit[A] = (s, a) => List(a)

    def flip[R]() = state => resume => for {
      ts <- resume(true)(state)
      fs <- resume(false)(state)
    } yield ts ++ fs
  }
}
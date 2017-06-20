package effekt
package effects

trait Amb extends Eff {
  def flip[R](): Boolean @@ R
}
object Amb {
  def flip()(implicit u: Use[Amb]): Control[Boolean] =
    use(u)(u.effect.flip())

  object ambList extends Amb {
    type Out[A] = List[A]
    def unit[A] = a => List(a)

    def flip[R]() = resume => for {
      ts <- resume(true)
      fs <- resume(false)
    } yield ts ++ fs
  }
}
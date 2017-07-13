package effekt
package effects

trait Amb extends Eff {
  def flip(): CPS[Boolean]
}
object Amb {
  def flip()(implicit u: Use[Amb]): Control[Boolean] =
    use(u)(u.effect.flip())

  trait AmbList extends Amb {
    type Res = List[R]
    def unit = a => List(a)

    def flip() = resume => for {
      ts <- resume(true)
      fs <- resume(false)
    } yield ts ++ fs
  }
  def ambList[R0] = new AmbList { type R = R0 }
}
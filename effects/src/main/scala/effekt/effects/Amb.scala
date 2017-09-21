package effekt
package effects

trait Amb extends Eff {
  def flip(): Op[Boolean]
}
object Amb {
  def flip()(implicit u: Use[Amb]): Control[Boolean] =
    use(u)(u.effect.flip())

  def ambList[R] = new Handler.Basic[R, List[R]] with Amb {
    def unit = a => List(a)

    def flip() = _ => resume => for {
      ts <- resume(true)(())
      fs <- resume(false)(())
    } yield ts ++ fs
  }
}
package effekt
package effects

trait Amb extends Eff {
  def flip(): Op[Boolean]
}
object Amb {
  trait AmbList[R] extends Amb with Handler.Aux[R, List[R]] {
    def unit = r => List(r)

    def flip() = resume => for {
      ts <- resume(true)
      fs <- resume(false)
    } yield ts ++ fs
  }

  // Boilerplate:
  def ambList[R](f: Use[Amb] => Control[R]) = handle(new AmbList[R] {})(f)

  def flip()(implicit u: Use[Amb]): Control[Boolean] =
    use(u)(u.effect.flip())
}

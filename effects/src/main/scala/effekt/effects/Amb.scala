package effekt
package effects

trait Amb extends Eff {
  def flip(): Op[Boolean]
}
object Amb {
  trait AmbList extends Amb with Handler {
    type Res = List[R]
    def unit = a => List(a)

    def flip() = resume => for {
      ts <- resume(true)
      fs <- resume(false)
    } yield ts ++ fs
  }

  // Boilerplate:
  def ambList[R0](
   f: Capability { val effect: Amb { type R = R0 } } => Control[R0]
  ) = handle(new AmbList { type R = R0 })(f)

  def flip()(implicit u: Use[Amb]): Control[Boolean] =
    use(u)(u.effect.flip())
}
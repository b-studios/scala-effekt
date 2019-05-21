package effekt
package effects

trait Amb {
  def flip(): Control[Boolean]
}
object Amb {

  def ambList[R] = new Handler[R, List[R]] with Amb {
    def unit = a => pure(List(a))

    def flip() = use { resume => for {
      ts <- resume(true)
      fs <- resume(false)
    } yield ts ++ fs }
  }
}
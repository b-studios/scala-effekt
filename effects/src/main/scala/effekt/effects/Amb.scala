package effekt
package effects

trait Amb {
  def flip(): Control[Boolean]
}
object Amb {
  def ambList[R](prog: Amb => Control[R]): Control[List[R]] =
    new Handler[List[R]] with Amb {
      def flip() = use { resume => for {
         ts <- resume(true)
          fs <- resume(false)
        } yield ts ++ fs
      }
    } handle { amb => prog(amb) map { r => List(r) } }
}
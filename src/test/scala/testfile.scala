import scala.util.escape._

import effekt._

trait Amb extends Eff {
  def flip[R](): Boolean @@ R
}
object Amb {
  def flip()(implicit @local u: Use[Amb]): Control[Boolean] =
    use(u) { u.effect.flip() }
}
import Amb._

object ambList extends Amb {
  type State = Unit
  type Out[A] = List[A]
  def unit[A] = (s, a) => List(a)
  def flip[R]() = state => resume => for {
    x <- resume(true)(state)
    y <- resume(false)(state)
  } yield (x ++ y)
}

object testApp extends App {

  var esc: Use[Amb] = null

  val res = handle(ambList) { implicit a =>
    flip().flatMap { x =>
      flip().map { y =>
        // esc = a // or returning a will fail!
        x || y
      }
    }
  }

  println(res.run())
}
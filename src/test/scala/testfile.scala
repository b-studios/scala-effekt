import scala.util.escape._

import effekt._
import effects._
import Amb._

object testApp extends App {

  var x: Use[Amb] = null

  val res = handle(ambList) (new (Use[Amb] -> Control[Boolean]) {
    def apply(@local a: Use[Amb]) = {
      implicit @local val av = a
//      x = a // won't compile
      flip()
    }
  })

  println(res.run())
}
import scala.util.escape._

import effekt._
import effects._
import Amb._

object testApp extends App {

  var x: Use[Amb] = null

  val res = handle(ambList) { a =>
    flip()(a).flatMap { x =>
      flip()(a).map { y =>
        x || y
      }
    }
  }

  println(res.run())
}
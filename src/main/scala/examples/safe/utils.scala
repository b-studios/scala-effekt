package examples.safe

import effekt._

object utils {
  def log(msg: Any): Unit / Pure = pure { println(msg) }
}
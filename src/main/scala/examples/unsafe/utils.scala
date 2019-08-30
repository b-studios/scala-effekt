package examples.unsafe

import effekt.unsafe._

object utils {
  def log(msg: Any): Control[Unit] = pure { println(msg) }
}
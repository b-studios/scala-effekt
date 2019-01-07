package effekt
package examples

object utils {
  def log(msg: String): Unit / Any = pure { println(msg) }
}
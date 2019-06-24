package effekt
package examples

object utils {
  def log(msg: String): Unit / Pure = pure { println(msg) }
}
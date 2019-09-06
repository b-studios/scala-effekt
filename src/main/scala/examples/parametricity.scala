package examples

import effekt._
import nondet._

object parametricity extends App {

  def delimit[R, E](s: Scope[R, E])(prog: R / s.effect): R / (s.effect & E) =
    s.switch[R / (s.effect & E)] { resume =>
      resume(resume(prog))
    } flatMap { c => c }

  val ex = handle[String, Pure] { s =>
    delimit(s) {
      s.switch { resume => pure("abort") }
    } map { x => "Did not abort" }
  }

  println { run { ex } }
}
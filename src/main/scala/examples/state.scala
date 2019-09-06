package examples

import effekt._
import nondet._

object state extends App {
  val ex1 = collect { amb =>
    var x = 0
    amb.flip() map { b =>
      if (b) { x = 2 } else {}
      x
    }
  }

  println { run { ex1 } }

  val ex2 = collect { amb => region[Int, amb.effect] { state =>
    val x = state.Field(0)
    amb.flip() flatMap { b =>
      if (b) x.put(2) else pure(())
    } andThen x.get()
  }}

  println { run { ex2 } }
}
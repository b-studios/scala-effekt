package examples

import effekt._

object delimitedcontrol extends App {

  val ex1: Unit / Pure = handle[Int, Pure] { scope =>
    scope.switch[Int] { resume => for {
      x <- resume(3)
      y <- resume(4)
    } yield x + y } map { x => (1 + x) * 2 }
  } map { x => print(x) }

  run { ex1 }


  val ex2: Int / Pure = handle { (s1: Scope[Int, Pure ]) =>
    handle { (s2 : Scope[Boolean, s1.effect]) =>
      s1.switch[Boolean] { resume => pure(21) } // Control[Int, s2.effect & s1.effect]
    } map { x ⇒ if (x) 1 else 2 }      // Control[Int, s1.effect]
  } map { x ⇒ 2 * x }                  // Control[Int, Pure]

}
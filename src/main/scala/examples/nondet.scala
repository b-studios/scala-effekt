package examples

import effekt._

object nondet extends App {

  trait Exc extends Eff { def raise(msg: String): Nothing / effect }
  def Exc(using e: Exc): e.type = e

  trait Amb extends Eff { def flip(): Boolean / effect }
  def Amb(using a: Amb): a.type = a

  def drunkFlip(amb: Amb, exc: Exc): Control[String, exc.effect & amb.effect] = for {
    caught <- amb.flip()
    r <- if (caught) amb.flip() else exc.raise("oops")
  } yield if (r) "heads" else "tails"

  def drunkFlipI(using Exc, Amb) = for {
    caught <- Amb.flip()
    r <- if (caught) Amb.flip() else Exc.raise("oops")
  } yield if (r) "heads" else "tails"


  // Handlers from the Introduction
  // ==============================
  def maybe[R, E](prog: (exc: Exc) => R / (exc.effect & E)): Option[R] / E = // (1) handlers introduce a new scope
    handle { scope =>
      // (2) create handler instance / capability
      val exc = new Exc {
        // (3) communicate the use of ‘scope‘
        type effect = scope.effect
        // (4) implement effect operations
        def raise(msg: String) = scope.switch { resume ⇒ pure(None) }
      }
      // (5) provide handled program with capability
      prog(exc) map { r => Some(r) }
      //        ^^^ (6) lift pure values into the effect domain
    }

  def collect[R, E](prog: (amb: Amb) ⇒ R / (amb.effect & E)): List[R] / E = handle { scope =>
    val amb = new Amb {
      type effect = scope.effect
      def flip() = scope.switch { resume =>
        for { xs <- resume(true); ys <- resume(false) } yield xs ++ ys }
      }
    prog(amb) map { r => List(r) }
  }

  val res1 = run {
    collect { amb =>
      maybe[String, amb.effect] { exc =>
        drunkFlip(amb, exc) // Control[String, exc.effect & amb.effect]
      }                     // Control[Option[String], amb.effect]
    }                       // Control[List[Option[String]], Pure]
  }                         // List[Option[String]]
}
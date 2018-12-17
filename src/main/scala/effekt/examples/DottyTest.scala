package effekt
package examples
import utils._

object DottyTest extends App {

  // Effect Signatures
  trait Exc extends Eff {
    def raise(msg: String): Nothing / effect
  }

  trait Amb extends Eff {
    def flip(): Boolean / effect

    def choose[A, E](c1: A / E, c2: A / E): A / (effect & E) = for {
      b   <- flip()
      res <- if (b) c1 else c2
    } yield res
  }

  // Effect Usage

  def drunkFlip(amb: Amb, exc: Exc): String / (amb.effect & exc.effect) = for {
    caught <- amb.flip()
    heads <- if (caught) amb.flip() else exc.raise("Too drunk")
  } yield if (heads) "Heads" else "Tails"

  def drunkFlip2(amb: Amb, exc: Exc) = for {
    caught <- amb.flip()
    heads <- amb.choose(amb.flip(), exc.raise("Too drunk"))
  } yield if (heads) "Heads" else "Tails"


  lazy val x = 0

  def flipTwice(amb: Amb) =
    for {
      x <- amb.flip()
      y <- amb.flip()
    } yield x || y

  def prog(exc: Exc, amb: Amb) =
    if (x <= 0) amb.flip() else exc.raise("too big")

  def div(x: Int, y: Int)(exc: Exc) =
    if (y == 0) exc.raise("y is zero") else pure(x / y)


  // Effect Handlers
  trait Maybe[R, E] extends Exc with Handler[Option[R], E] {
    def raise(msg: String): Nothing / effect = effect { pure(None) }
  }

  trait ExcList[R, E] extends Exc with Handler[List[R], E] {
    def raise(msg: String): Nothing / effect = effect { pure(Nil) }
  }

  trait Both[R, E] extends ExcList[R, E] with AmbList[R, E] {
    override def flip(): Boolean / effect = raise("broken")
  }
  def Both[R, E](action: (exc: Exc, amb: Amb) => R / (amb.effect & exc.effect & E)): List[R] / E =
    handler { e =>
      val both = new Both[R, E] { val effect: e.type = e }
      action(both, both).map { r => List(r) }
    }

  def Maybe[R, E](action: (exc: Exc) => R / (exc.effect & E)): Option[R] / E =
    handler { e =>
      val exc = new Maybe[R, E] { val effect: e.type = e }
      action(exc).map { r => Some(r) }
    }

//  def Maybe2[R, E](action: (exc: Exc) => R / (exc.type & E)): Option[R] / E =
//    handling[Option[R], E] { p =>
//      action(msg => use(p) in { pure(None) }).map { r => Some(r) }
//    }

  trait AmbList[R, E] extends Amb with Handler[List[R], E] {
    def flip(): Boolean / effect = effect {
      for {xs <- resume(true); ys <- resume(false)}
        yield xs ++ ys
    }
  }

  def AmbList[R, E](action: (amb: Amb) => R / (amb.effect & E)): List[R] / E =
    handler { e =>
      val amb = new AmbList[R, E] { val effect: e.type = e }
      action(amb).map { x => List(x) }
    }

  // Handling of Effects

  // removing the type annotations gives "unspecified error"
  val res2 = run { AmbList [Boolean, Pure] { amb => flipTwice(amb) } }

  println(res2)

  val res3 = run {
    AmbList [Option[Boolean], Pure] { amb =>
      Maybe { exc =>
        prog(exc, amb)
      }
    }
  }

  println(res3)

  val res4 = run {
    Maybe [List[Boolean], Pure] { exc =>
      AmbList { amb =>
        prog(exc, amb)
      }
    }
  }

  println(res4)

  val res6: Option[List[String]] = run {
    Maybe [List[String], Pure] { exc =>
      AmbList { amb =>
        drunkFlip(amb, exc)
      }
    }
  }

  println(res6)

  val res7: List[Option[String]] = run {
    AmbList [Option[String], Pure] { amb =>
      Maybe { exc =>
        drunkFlip(amb, exc)
      }
    }
  }

  println(res7)

//  var escaped: Amb = null

  // this should not typecheck...
//  val res5 = Maybe { exc =>
//    for {
//      _ <- AmbList { amb =>
//          escaped = amb
//          pure(true)
//      }
//      res <- AmbList { amb2 =>
//          prog(escaped)(exc)
//      }
//    } yield res
//  }.run


  // effectful traversals
  trait Exp[-I, +O] {
    def Lit: Int => O
    def Add: (I, I) => O
  }

  trait Binding[-I, +O] {
    def Var: String => O
    def Let: (String, I) => O
  }

  // E is invariant since it is used both on I and O
  trait Effectful[-I, +O, E] extends Binding[I / E, O / E]
}
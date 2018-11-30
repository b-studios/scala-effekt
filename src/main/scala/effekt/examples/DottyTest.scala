package effekt
package examples
import utils._

object DottyTest extends App {

  // Effect Signatures
  trait Exc {
    def raise(msg: String): Nothing / this.type
  }

  trait Amb {
    def flip(): Boolean / this.type

    def choose[A, E](c1: A / E, c2: A / E): A / (this.type & E) = for {
      b   <- flip()
      res <- if (b) c1 else c2
    } yield res
  }

  // Effect Usage

  def drunkFlip(amb: Amb, exc: Exc) = for {
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
    def raise(msg: String): Nothing / this.type = use { pure(None) }
  }

  trait ExcList[R, E] extends Exc with Handler[List[R], E] {
    def raise(msg: String): Nothing / this.type = use { pure(Nil) }
  }

  trait Both[R, E] extends ExcList[R, E] with AmbList[R, E] {
    override def flip(): Boolean / this.type = raise("broken")
  }
  def Both[R, E](action: (exc: Exc, amb: Amb) => R / (amb.type & exc.type & E)): List[R] / E =
    handle(new Both[R, E] {}) { both =>
      action(both, both).map { r => List(r) }
    }

  def Maybe[R, E](action: (exc: Exc) => R / (exc.type & E)): Option[R] / E =
    handle(new Maybe[R, E] {}) { exc => action(exc).map { r => Some(r) } }

//  def Maybe2[R, E](action: (exc: Exc) => R / (exc.type & E)): Option[R] / E =
//    handling[Option[R], E] { p =>
//      action(msg => use(p) in { pure(None) }).map { r => Some(r) }
//    }

  trait AmbList[R, E] extends Amb with Handler[List[R], E] {
    def flip(): Boolean / this.type = use {
      for {xs <- resume(true); ys <- resume(false)}
        yield xs ++ ys
    }
  }

  def AmbList[R, E](action: (amb: Amb) => R / (amb.type & E)): List[R] / E =
    handle(new AmbList[R, E] {}) { amb => action(amb).map { x => List(x) } }

  // Handling of Effects

  val res = handle(new Maybe[Int, Pure] {}) { exc =>
    for {
      r <- div(1, 0)(exc)
    } yield Some(r)
  }.run

  println(res)

  // removing the type annotations gives "unspecified error"
  val res2 = AmbList [Boolean, Pure] { amb => flipTwice(amb) }.run

  println(res2)

  val res3 = AmbList [Option[Boolean], Pure] { amb =>
    Maybe { exc =>
      prog(exc, amb)
    }
  }.run

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



  // Alternative, not fixing the effect to this.type. Fixing the type
  // forces handler implementors to use inheritance instead of composition.
  object alternative {

    type @@[A, E] = A & Eff { type effect = E }

    // helper for precise typing
    def effect[E <: Eff](p: Prompt)(impl: E { type effect = p.type }): E { type effect = p.type } = impl

    trait Amb extends Eff { def flip(): Boolean / effect }
    trait Exc extends Eff { def raise(msg: String): Nothing / effect }

    def drunkFlip(amb: Amb, exc: Exc): String / (amb.effect & exc.effect) = for {
        caught <- amb.flip()
        heads <- if (caught) amb.flip() else exc.raise("Too drunk")
      } yield if (heads) "Heads" else "Tails"

    def handler[R, E](action: (amb: Amb, exc: Exc) => R / (amb.effect & exc.effect & E)): List[R] / E = handling { p =>
      action(effect[Amb](p) { () => use(p) in {
        for { xs <- resume(true); ys <- resume(false) }
          yield xs ++ ys
      }}, effect[Exc](p) { msg => use(p) in {
        pure(List())
      }}).map { List(_) }
    }

    run {
      handler[String, Pure] { (amb, exc) =>
        drunkFlip(amb, exc)
      }
    }



    val res: Amb / Pure = handling { implicit p =>
      // here we need to teach Dotty the refinement that amb.effect = p.type
      val amb: Amb { type effect = p.type } = () => use(p) in { resume(true) }

      // while
      implicitly[Amb { type effect = p.type } =:= (Amb @@ p.type)]

      // we can't use
      // that's a bug in Dotty. Probably to little dealiasing
      //   val amb2: Amb @@ p.type = () => use(p) in { resume(true) }

      val amb3 = effect[Amb](p) { () => use in { resume(true) } }

      amb.flip()
      pure(amb3)
    }

    // does not typecheck since `Boolean / Pure` != `Boolean / amb.effect`
//    val escaped: Boolean / Pure = res flatMap { amb => amb.flip() }
  }
}
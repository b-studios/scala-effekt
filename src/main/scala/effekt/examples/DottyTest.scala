package effekt
package examples

object DottyTest extends App {

  // Effect Signatures
  trait Exc {
    def raise[A](msg: String): A / this.type
  }

  trait Amb {
    def flip(): Boolean / this.type
  }

  // Effect Usage

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
    def raise[A](msg: String): A / this.type = use { pure(None) }
  }

  def Maybe[R, E](action: (exc: Exc) => R / (exc.type & E)): Option[R] / E =
    handle(new Maybe[R, E] {}) { exc => action(exc).map { r => Some(r) } }

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
    Maybe [Boolean, amb.type] { exc =>
      prog(exc, amb)
    }
  }.run

  println(res3)

  val res4 = run {
    Maybe [List[Boolean], Pure] { exc =>
      AmbList [Boolean, exc.type] {
        amb => prog(exc, amb)
      }
    }
  }

  println(res4)

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


  // Alternative, not fixing the effect to this.type. Fixing the type
  // forces handler implementors to use inheritance instead of composition.
  object alternative {

    trait Eff { type effect }

    type @@[A, E] = A & Eff { type effect = E }

    trait Amb extends Eff { def flip(): Boolean / effect }

    // helper for precise typing
    def effect[E <: Eff](p: Prompt)(impl: E { type effect = p.type }): E { type effect = p.type } = impl

    val res: Amb / Pure = handling { implicit p =>
      // here we need to teach Dotty the refinement that amb.effect = p.type
      val amb: Amb { type effect = p.type } = () => use in { resume(true) }

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
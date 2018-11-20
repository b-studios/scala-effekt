package effekt
package examples

import effekt._

object DottyTest extends App {

  // Effect Signatures
  trait Exc {
    def raise[A](msg: String): A / this.type
  }

  trait Amb {
    def flip(): Boolean / this.type
  }

  // Some Boilerplate
  def Exc(implicit exc: Exc): exc.type = exc
  def Amb(implicit amb: Amb): amb.type = amb


  // Effect Usage

  lazy val x = 0

  val flipTwice: Prog[Boolean using Amb] =
    for {
      x <- Amb.flip()
      y <- Amb.flip()
    } yield x || y

  lazy val prog: Prog[Boolean using Exc and Amb] = implicit (a: Amb) => implicit (e: Exc) =>
    if (x <= 0) Amb.flip() else Exc.raise("too big")

  def div(x: Int, y: Int): Prog[Int using Exc] =
    if (y == 0) Exc.raise("y is zero") else pure(x / y)


  // Effect Handlers
  trait Maybe[R, E] extends Exc with Handler.Base[Option[R], E] {
    def raise[A](msg: String): A / this.type = use { pure(None) }
  }

  def Maybe[R, E](f: Prog[R using Exc also E]): Option[R] / E = handle(new Maybe[R, E] {}) { implicit exc =>
    f(exc).map { r => Some(r) }
  }

  trait AmbList[R, E] extends Amb with Handler.Base[List[R], E] {
    def flip(): Boolean / this.type = use {
      for { xs <- resume(true); ys <- resume(false) }
        yield xs ++ ys
    }
  }
  def AmbList[R, E](f: Prog[R using Amb also E]): List[R] / E = handle(new AmbList[R, E] {}) { implicit amb =>
    f(amb).map { x => List(x) }
  }

  // Handling of Effects

  val res = handle(new Maybe[Int, Pure] {}) {
    for {
      r <- div(1, 0)(implicitly)
    } yield Some(r)
  }.run

  println(res)

  // un-eta-expanding gives "unspecified error"
  val res2 = AmbList { implicit (amb: Amb) => flipTwice(amb) }.run

  println(res2)

  val res3 = AmbList { implicit (amb: Amb) =>
    Maybe { implicit (exc: Exc) =>
      prog(amb)(exc)
    }
  }.run

  println(res3)

  val res4 = Maybe { implicit (exc: Exc) =>
    AmbList { implicit (amb: Amb) =>
      prog(amb)(exc)
    }
  }.run

  println(res4)

  var escaped: Amb = null

  // this should not typecheck...
//  val res5 = Maybe { implicit (exc: Exc) =>
//    for {
//      _ <- AmbList {
//        implicit (amb: Amb) =>
//          escaped = amb
//          pure(true)
//      }
//      res <- AmbList {
//        implicit (amb2: Amb) =>
//          prog(escaped)(exc)
//      }
//    } yield res
//  }.run
}
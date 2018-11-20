package effekt
package examples

import effekt._

object DottyTest extends App {

  lazy val x = 0

  val flipTwice: Prog[Boolean using Amb] =
    for {
      x <- Amb.flip()
      y <- Amb.flip()
    } yield x || y

  lazy val prog: Prog[Boolean using Exc and Amb] = implicit (a: Amb) => implicit (e: Exc) =>
    if (x <= 0) Amb.flip() else Exc.raise("too big")
//
//  lazy val res_1: Control[List[Option[Boolean]]] = AmbList { Maybe { prog } }
//  lazy val res_2: Control[Option[List[Boolean]]] = Maybe { AmbList { prog } }
//
//
  def div(x: Int, y: Int): Prog[Int using Exc] =
    if (y == 0) Exc.raise("y is zero") else pure(x / y)



  trait Exc {
    def raise[A](msg: String): A / this.type
  }
  def Exc(implicit exc: Exc): exc.type = exc

  trait Maybe[R] extends Exc with Handler {
    type Res = Option[R]
    def raise[A](msg: String): A / this.type = use { pure(None) }
  }

  def Maybe[R, E](f: Prog[R using Exc also E]): Option[R] / E = handle(new Maybe[R] { type Effects = E }) { implicit exc =>
    f(exc).map { r => Some(r) }
  }

  trait Amb {
    def flip(): Boolean / this.type
  }
  def Amb(implicit amb: Amb): amb.type = amb

  trait AmbList[R] extends Amb with Handler {
    type Res = List[R]

    def flip(): Boolean / this.type = use {
      for { xs <- resume(true); ys <- resume(false) }
        yield xs ++ ys
    }
  }
  def AmbList[R, E](f: Prog[R using Amb also E]): List[R] / E = handle(new AmbList[R] { type Effects = E })(implicit amb =>
    f(amb).map { x => List(x) }
  )

  val res = handle(new Maybe[Int] { type Effects = Pure }) {
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
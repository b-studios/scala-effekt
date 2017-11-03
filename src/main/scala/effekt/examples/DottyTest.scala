package effekt
package examples

import effekt._

object DottyTest extends App {

  lazy val x = 0

  lazy val prog: Boolean using Amb and Exc =
    if (x <= 0) flip() else raise("too big")

  lazy val res_1: Control[List[Option[Boolean]]] = AmbList { Maybe { prog } }
  lazy val res_2: Control[Option[List[Boolean]]] = Maybe { AmbList { prog } }


  def div(x: Int, y: Int): Int using Exc =
    if (y == 0) raise("y is zero") else pure(x / y)

  trait Exc extends Eff {
    def raise[A](msg: String): Op[A]
  }

  def raise[A](msg: String): A using Exc =
    implicit e => use(e)(e.handler.raise(msg))

  trait Maybe[R] extends Exc with Handler.Basic[R,  Option[R]] {
    def unit = r => Some(r)
    def raise[A](msg: String) = pure(None)
  }
  def Maybe[R](f: R using Exc) = handle(new Maybe[R] {  })(f)

  trait Amb extends Eff {
    def flip(): Op[Boolean]
  }

  trait AmbList[R] extends Amb with Handler.Basic[R,  List[R]] {
    def unit = r => List(r)
    def flip() = for { xs <- resume(true);  ys <- resume(false) }
      yield xs ++ ys
  }
  def flip(): Boolean using Amb =
    implicit a => use(a)(a.handler.flip())

  def AmbList[R](f: R using Amb) = handle(new AmbList[R] {})(f)

  println(run { Maybe { div(0, 4) } })
  println(run { Maybe { div(4, 0) } })

  println(run { res_1 })
  println(run { res_2 })
}
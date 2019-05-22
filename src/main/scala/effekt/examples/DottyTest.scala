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

  trait Exc {
    def raise[A](msg: String): Control[A]
  }

  def raise[A](msg: String): A using Exc = given e => e.raise(msg)

  trait Maybe[R] extends Exc with Handler[Option[R]] {
    def raise[A](msg: String) = use { pure(None) }
  }
  def Maybe[R](prog: R using Exc) = handle(new Maybe[R] {}) {
    prog map { r => Some(r) }
  }

  trait Amb {
    def flip(): Control[Boolean]
  }

  trait AmbList[R] extends Amb with Handler[List[R]] {
    def flip() = use {
      for { xs <- resume(true); ys <- resume(false) }
        yield xs ++ ys
    }
  }
  def flip(): Boolean using Amb = given a => a.flip()

  def AmbList[R](prog: R using Amb) = handle(new AmbList[R] {}) {
    prog map { r => List(r) }
  }

  println(run { Maybe { div(0, 4) } })
  println(run { Maybe { div(4, 0) } })

  println(run { res_1 })
  println(run { res_2 })

  val stateTest = AmbList {
    region {
      val s1 = Field(1)
      val s2 = Field(2)

      for {
        _ <- s1 update { _ + 1 }
        b <- flip()
        r <- if (b) for {
          _ <- s2 update { _ + 1 }
          x <- s1.value
          y <- s2.value
        } yield x + y else for {
          x <- s1.value
          y <- s2.value
        } yield x + y
      } yield r
    }
  }

  println(run { stateTest }) // List(5, 4)

  trait Out[A] {
    def out(a: A): Control[Unit]
  }
  def out[A](a: A) given (o: Out[A]) = o.out(a)

  class ListWriter[R, A] extends Handler.Stateful[R, List[A]](Nil) with Out[A] {
    // Oh. Now it would be helpful to have the unit / return clauses again
    // to return the state when we are done.
    def out(a: A) = useState { s => resume((), a :: s) }
  }

  // it is still possible, but feels a bit more akward.
  def ListWriter[R, A](prog: R using Out[A]): Control[(R, List[A])] = {
    val writer = new ListWriter[R, A]
    for {
      r <- handle(writer) { prog }
      s <- writer.state.value
    } yield (r, s)
  }

  val generator: Unit using Out[Int] = for {
    _ <- out(1)
    _ <- out(2)
    _ <- out(3)
    _ <- out(4)
    _ <- out(5)
  } yield ()

  println { run { ListWriter { generator } } }
}
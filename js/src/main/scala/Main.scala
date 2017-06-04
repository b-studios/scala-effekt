package effekt

import scala.scalajs.js

trait Amb extends Eff { self =>
  def flip[R](): Boolean @@ R
}
object Amb {
  def flip()(implicit u: Use[Amb]) = use(u) { u.effect.flip() }
}

object ambHandler extends Amb {
  type Out[A] = List[A]
  type State = Unit
  def flip[R]() = s => resume =>
    for {
      t <- resume(true)(s)
      f <- resume(false)(s)
    } yield t ++ f
  def unit[A] = (s, a) => List(a)
}

trait State[S] extends Eff {
  def get[R](): S @@ R
  def put[R](s: S): Unit @@ R
}

object State {
  def get[S]()(implicit u: Use[State[S]]): Control[S] = use(u) { u.effect.get() }
  def put[S](s: S)(implicit u: Use[State[S]]) = use(u) { u.effect.put(s) }
}

class StateHandler[S] extends State[S] {
  type Out[A] = A
  type State = S
  def get[R]() = s => resume => resume(s)(s)
  def put[R](s: S) = _ => resume => resume(())(s)
  def unit[A] = (s, a) => a
}
object StateHandler {
  def apply[S] = new StateHandler[S]
}

import Amb._, State._
object Main extends js.JSApp {


  def flipCounter(implicit s: Use[State[Int]], amb: Use[Amb]): Control[Boolean] =
    get().flatMap { c =>
      if (c <= 0) {
        flip()
      } else {
        for {
          _ <- put(c - 1)
          res <- flipCounter.map { x => !x }
        } yield res
      }
    }

  def flipNState(n: Int): List[Boolean] =
    handle(ambHandler) { implicit a =>
      handle(StateHandler[Int])(n) { implicit s =>
        flipCounter
      }
    }.run()

  def example(implicit u1: Use[State[Int]], u2: Use[Amb]): Control[Int] = for {
    x <- get()
    b <- flip()
    _ <- if (b) put(x + 1) else pure(())
    y <- get()
  } yield (x + y)

  val result1 = handle(ambHandler) { implicit a =>
    handle(StateHandler[Int])(0) { implicit s =>
      example
    }
  }
  val result2 = handle(StateHandler[Int])(0) { implicit s =>
    handle(ambHandler) { implicit a =>
      example
    }
  }

  def flipN(n: Int)(implicit amb: Use[Amb]): Control[Boolean] =
    if (n == 0) {
      pure(false)
    } else for {
      x <- flip()
      y <- flipN(n - 1)
    } yield x || y

  def main(): Unit = {
    val before = System.currentTimeMillis()
    for (i <- 1 to 1000) {
      val l = flipNState(1000)
    }
    val after = System.currentTimeMillis()
    println(after - before)
  }
}

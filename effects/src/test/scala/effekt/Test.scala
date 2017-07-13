package effekt

object Test extends App {

  import effects._
  import State._
  import Amb._

  def prog(implicit s: Use[State[Int]]): Control[(Int, Int)] = for {
    x <- s.value
    _ <- s.value = 42
    y <- s.value
  } yield (x, y)

  val res = handle(state[Int, (Int, Int)]) { implicit s => prog }.runState(0)

  def flipCounter(implicit s: Use[State[Int]], ua: Use[Amb]): Control[Boolean] = {
    get().flatMap { c =>
      if (c <= 0) {
        flip()
      } else {
        for {
          _ <- s.value = c - 1
          res <- flipCounter
        } yield !res
      }
    }
  }

  def countTo(n: Int): List[Boolean] =
    (handle(ambList[Boolean]) { implicit a =>
      handle(state[Int, Boolean]) { implicit r1 =>
        flipCounter
      }.runState(n)
    }).run()

  val N = 10000
  val warmup = 100
  val rep = 100

  def runTest(): Long = {
    val before = System.currentTimeMillis()
    countTo(N)
    val after = System.currentTimeMillis()
    after - before
  }

  println((1 to (rep + warmup)).map(_ => runTest()).takeRight(rep).sum / rep.toFloat + "ms")

}
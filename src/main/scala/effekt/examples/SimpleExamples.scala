package effekt
package examples

import cats.Applicative
import cats.implicits._

import effekt.catsinterop._
import effekt.handler._

trait SimpleExamples {

  def expect[R](expected: R)(prog: C[R]): Unit = {
    val got = run { prog }
    assert(expected == got, s"Expected $expected but got $got")
  }

  trait Tick { def tick(): I[Unit] }
  def Tick: Tick using Tick = implicit t => t

  trait Put { def put(n: Int): I[Unit] }
  def Put: Put using Put = implicit p => p

  trait Get { def get(): I[Int] }
  def Get: Get using Get = implicit g => g

  val example: I[String] using Tick and Put = {
    Tick.tick() andThen Put.put(3) andThen Tick.tick() andThen Put.put(7) andThen pure("done")
  }

  val exampleGet: I[Int] using Get and Put = {
    Applicative[I].map3(Get.get(), Get.get(), Put.put(4) andThen Put.put(7)) {
      case (x, y, _) => x + y
    }
  }

  class CountTicks extends Tick with Idiomatic {
    type G[X] = (X, Int)
    def unit[R] = r => (r, 0)
    def map[A, B] = f => (a, n) => (f(a), n)
    def tick() = usePure { case (k, n) => (k(()), n + 1) }
  }
  def countTicks = new CountTicks

  class PrintTicks extends Tick with Monadic {
    type G[X] = X
    def unit[R] = r => r
    def tick() = use { resume =>
      println("tick")
      resume(())
    }
  }
  def printTicks = new PrintTicks

  class Get42 extends Get with Monadic {
    type G[X] = X
    def unit[R] = r => r
    def get() = use { resume => resume(42) }
  }
  def get42 = new Get42

  class SumPuts extends Put with Idiomatic {
    type G[X] = (X, Int)
    def unit[R] = r => (r, 0)
    def map[A, B] = f => (a, n) => (f(a), n)
    def put(n: Int) = usePure { case (k, m) => (k(()), m + n) }
  }
  def sumPuts = new SumPuts

  class OnlySum extends Put with Idiomatic {
    type G[X] = Int
    def unit[R] = r => 0
    def map[A, B] = f => n => n
    def put(n: Int) = usePure { _ + n }
  }
  def onlySum = new OnlySum

  class CountGet extends Get with Idiomatic {
    type G[X] = Int
    def unit[R] = r => 0
    def map[A, B] = f => n => n
    def get() = usePure { n => n + 1 }
  }
  def countGet = new CountGet

  class CountGet2 extends Get with Idiomatic {
    type G[R] = (R, Int)
    def unit[R] = r => (r, 0)
    def map[A, B] = f => (a, n) => (f(a), n)
    def get() = usePure { case (k, n) => (k(0), n + 1) }
  }
  def countGet2 = new CountGet2

  class Both extends Put with Get with Idiomatic {
    type G[X] = (X, Int)
    def unit[R] = r => (r, 0)
    def map[A, B] = f => (a, n) => (f(a), n)
    def get() = usePure { case (k, m) => (k(m), m) }
    def put(n: Int) = usePure { case (k, m) => (k(()), n) }
  }
  def both = new Both

  class MonadicGet[R] extends Get with Monadic {
    type G[X] = List[X]
    def unit[R] = r => List(r)

    def get() = use { resume =>
      for {
        xs <- resume(0)
        ys <- resume(1)
      } yield xs ++ ys
    }
  }
  def monadicGet[R](prog: C[R] using Get): C[List[R]] =
    new MonadicGet[R] handle { prog }

  expect ((2, 11)) {
    sumPuts { implicit _ =>
      countGet { implicit _ =>
        exampleGet
      }
    }
  }

  expect (((), 7)) {
    sumPuts { implicit _ =>
      Put.put(4) andThen Put.put(2) andThen Put.put(1)
    }
  }

  expect (10) {
    onlySum { implicit _ =>
      countTicks { implicit t => example }
    }
  }

  expect (("done", 10), 2) {
    countTicks { implicit _ =>
      sumPuts { implicit _ => example }
    }
  }

  val example1: C[Unit] using Put = for {
    _ <- Put.put(1)
    _ <- Put.put(2)
    _ <- pure(())
  } yield ()

  val example1a: C[Unit] using Put = Put.put(1)

  val example1b: C[Unit] using Put = Put.put(1) *> Put.put(2)

  val example1c: C[Unit] using Put =
    (Put.put(1) *> Put.put(2)).flatMap { _ => pure(()) }


  // a program that is in parts idiomatic.
  val example2: C[String] using Put = for {
    _ <- Put.put(3) andThen Put.put(4) andThen Put.put(5)
    x <- pure(0)
    _ <- Put.put(x + 1) andThen Put.put(x + 2) andThen Put.put(x + 3)
    y <- pure(1)
    _ <- Put.put(y + 1) andThen Put.put(y + 2) andThen Put.put(y + 3)
    _ <- pure(())
  } yield "done"

  type Trace = List[Int]
  def tracePuts[R](prog: C[R] using Put): C[(R, Trace)] =
    sumPuts.dynamic(prog(_) map { x => (x, List.empty[Int]) }) {
      (x, n) => resume => resume(x) map { case (r, ms) => (r, n :: ms) }
    }

  expect (((), List(1, 2))) {
    tracePuts { implicit _ =>
      example1
    }
  }

  expect (((), List(1))) {
    tracePuts { implicit _ =>
      example1a
    }
  }

  expect (((), List(3))) {
    tracePuts { implicit _ =>
      example1b
    }
  }

  expect (((), List(3))) {
    tracePuts { implicit _ =>
      example1c
    }
  }

  expect (("done", List(12, 6, 9))) {
    tracePuts { implicit _ =>
      example2
    }
  }

  val example3a: I[Unit] using Put and Get = Get.get() *> Put.put(3)
  val example3b: I[Int] using Put and Get = Put.put(3) *> Put.put(2) *> Get.get()

  val example3c: C[Unit] using Put and Get = for {
    x <- Get.get()
    _ <- Put.put(x + 1) *> Put.put(x + 2) *> Put.put(x + 3)
    x <- Get.get()
    _ <- Put.put(x + 1) *> Put.put(x + 2) *> Put.put(x + 3)
    _ <- pure(())
  } yield ()


  val example4: I[Unit] using Get and Tick =
    Tick.tick() andThen Tick.tick() andThen Tick.tick() andThen Get.get() andThen Tick.tick()

  expect (List(((),3), ((),3))) {
    monadicGet { implicit _ =>
      sumPuts { implicit _ =>
        example3a
      }
    }
  }

  expect (List((0,5), (1,5))) {
    monadicGet { implicit _ =>
      sumPuts { implicit _ =>
        example3b
      }
    }
  }

  val result3c = List(
    ((),List(6, 6)),
    ((),List(6, 9)),
    ((),List(9, 6)),
    ((),List(9, 9)))

  expect (result3c) {
    monadicGet { implicit _ =>
      tracePuts { implicit _ =>
        example3c
      }
    }
  }

  println { run {
    printTicks { implicit _ =>
      get42 { implicit _ =>
        example4
      }
    }
  }}

  println { run {
    printTicks { implicit _ =>
      onlySum { implicit _ =>
        example
      }
    }
  }}
}
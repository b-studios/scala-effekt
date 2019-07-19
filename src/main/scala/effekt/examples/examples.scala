package effekt

import cats.{ Applicative, Functor, Monoid, Monad }
import cats.implicits._

package object examples extends App {

  // Examples
  // ========

  case object Get extends Op[Int]
  def get(): Idiom[Int] = send(Get)

  case class Put(n: Int) extends Op[Unit]
  def put(n: Int): Idiom[Unit] = send(Put(n))

  case object Tick extends Op[Unit]
  def tick(): Idiom[Unit] = send(Tick)

  type Id[A] = A

  // A simple monadic handler that prints the values it receives
  def PrintPuts[R] = handler[R] {
    case Put(n) -> resume => println(n); resume(())
  }

  // A handler that constantly returns 42
  def Get42[R] = handler[R] {
    case Get -> resume => resume(42)
  }


  type WithInt[X] = (X, Int)

  // An idiomatic handler for put, that statically sums all the puts.
  object SumPuts extends Idiomatic[WithInt] {
    def onPure[R] = _ map { r => (r, 0) }
    def map[A, B] = (a, n) => f => (f(a), n)
    def onEffect[X, R] = lifted {
      case Put(n) => (k, m) => (k(()), m + n)
    }
  }

  // An idiomatic handler for get, that statically counts all the puts.
  object CountGets extends Idiomatic[WithInt] {
    def onPure[R] = _ map { r => (r, 0) }
    def map[A, B] = (a, n) => f => (f(a), n)
    def onEffect[X, R] = lifted {
      // here we make up a value (0) to continue. We could also pick `[X] => Int` to avoid this.
      case Get => (k, m) => (k(0), m + 1)
    }
  }

  type Trace = List[Int]

  // Lifts an idiomatic handler that statically computes an Int to
  // a dynamic trace over all idiomatic subprograms.
  def trace[R](h: Idiomatic[WithInt]): Eff[R] => Eff[(R, Trace)] = prog =>
    h.dynamic(prog map { r => (r, List.empty[Int]) })(new Sequencer {
      def apply[X] = (x, n) => resume => resume(x) map { case (r, ms) => (r, n :: ms) }
    })

  def traceGets[R] = trace[R](CountGets)
  def tracePuts[R] = trace[R](SumPuts)

  // A purely idiomatic program
  def prog(n: Int): Idiom[Int] =
    put(n + 1) andThen put(n + 2) andThen put(n + 3) andThen get()


  println { run { Get42 { PrintPuts { embed(prog(0)) } } } }
  // 1
  // 2
  // 3
  //> 42

  println { run { Get42 { embed { SumPuts { prog(0) } } } } }
  //> (42, 6)

  // Doesn't type check since Get42 is a *monadic* handler
  // and we don't have idiom-polymorphism.
  //   println { run { SumPuts { Get42 { prog(0) } } } }

  // however, we can use the tracing variant of puts:
  println { run { Get42 { trace(SumPuts) { embed { prog(0) } } } } }
  //> (42, List(6))

  // now also works in this nesting:
  println { run { trace(SumPuts) { Get42 { embed { prog(0) } } } } }
  //> (42, List(6))


  // A slightly larger example to illustrate "trace":
  val prog2 = for {
    x <- embed { prog(0) }
    y <- embed { prog(1).map2(get()) { _ + _ } }
    z <- embed { prog(2) }
  } yield (x, y, z)

  println { run { Get42 { trace(SumPuts) { prog2 } } } }
  //> ((42,42,42), List(6, 9, 12))

  println { run { trace(CountGets) { trace(SumPuts) { prog2 } } } }
  //> (((0,0,0), List(6, 9, 12)), List(1, 2, 1))
  //    ^^^^^^     ^^^^^^^^^^     ^^^^^^^^^^^^
  //  the result  the put trace   the get trace


  // A handler for tick, that calls the continuation twice
  def AmbiguousTick[R] = handler [R, List[R]] (x => Eff.pure(List(x))) {
    case Tick -> resume =>
      println("tick")
      for {
        first <- resume(())
        second <- resume(())
      } yield first ++ second
  }

  println { run { AmbiguousTick { embed { tick() } }}}
  //> List((), ())

  println { run { AmbiguousTick { trace(SumPuts) {
    embed { tick() andThen put(2) }
  }}}}
  //> List(((),List(2)), ((),List(2)))

  println { run { trace(SumPuts) { AmbiguousTick {
    embed { tick() andThen put(2) }
  }}}}
  //> (List((), ()), List(2, 2))

  // A larger program with nested idiomatic computation
  val prog3: Eff[(Int, Int, Int)] = for {
    x <- embed { prog(0) }
    y <- embed { put(1337) } andThen (for {
      a <- embed { tick() andThen List(10,20,30).traverse(prog) andThen get() }
      b <- embed { tick() andThen prog(2) }
    } yield a + b)
    z <- embed { get() andThen prog(3) }
  } yield (x, y, z)

  println("--------------------")
  println { run { AmbiguousTick { Get42 { tracePuts { prog3 } } } } }
  //> List(((42,84,42),List(6, 1337, 198, 12, 15)),
  //       ((42,84,42),List(6, 1337, 198, 12, 15)),
  //       ((42,84,42),List(6, 1337, 198, 12, 15)),
  //       ((42,84,42),List(6, 1337, 198, 12, 15)))


  val prog4 = embed { get() andThen put(20) andThen get() }

  println("--------------------")
  println { run { Get42 { tracePuts { prog4 } } } }

//
//  println("--------------------")
//  println { run { Get42 { AmbiguousTick { tracePuts { prog3 } } } } }
//  println("--------------------")
//  println { run {  tracePuts { Get42 { AmbiguousTick { prog3 } } } } }
}

package effekt
package free

import scala.annotation.tailrec

// Base: unsafe free(er) monad in 50 lines of code.
package object unsafe extends App {

  // for now we don't use union types but accept effect-unsafety

  type Continuation[A, B] = A => Eff[B]

  sealed trait Eff[A] {
    def map[B](f: A => B): Eff[B]
    def flatMap[B](f: A => Eff[B]): Eff[B]
    def ap[B](f: Eff[A => B]): Eff[B] = flatMap { a => f map { _ apply a } }
    def andThen[B](f: Eff[B]): Eff[B] = ap(f map { b => a: A => b })
  }
  case class Pure[A](value: A) extends Eff[A] {
    def map[B](f: A => B): Eff[B] = Pure(f(value))
    def flatMap[B](f: A => Eff[B]): Eff[B] = f(value)
  }
  case class Impure[X, A](op: Op[X], continuation: Continuation[X, A]) extends Eff[A] {
    def map[B](f: A => B): Eff[B] = Impure(op, x => continuation(x) map f)
    def flatMap[B](f: A => Eff[B]): Eff[B] = Impure(op, x => continuation(x) flatMap f)
  }

  def pure[A](a: A): Eff[A] = Pure(a)
  def run[A](ma: Eff[A]): A = ma match {
    case Pure(a) => a
    case _ => sys error "Cannot run program with unhandled effects: " + ma
  }

  // it's purposfully not sealed.
  trait Op[X]
  def send[X](op: Op[X]): Eff[X] = Impure(op, pure)

  // interpreters are *partial* functions: Op ~> Domain
  type ~>[-A, +B] = PartialFunction[A, B]

  trait Interpreter[G[_]] {
    def onPure[A]: A => Eff[G[A]]
    def onEffect[X, R]: Op[X] ~> (Continuation[X, G[R]] => Eff[G[R]])

    def apply[R](prog: Eff[R]): Eff[G[R]] = runInterpreter(this)(prog)
  }

  def runInterpreter[G[_], R](interpreter: Interpreter[G])(prog: Eff[R]): Eff[G[R]] =
    reset(interpreter) { prog flatMap interpreter.onPure }

  def reset[G[_], R](interpreter: Interpreter[G])(prog: Eff[G[R]]): Eff[G[R]] =
    prog match {
      case p @ Pure(a) => p
      case Impure(op, k) if interpreter.onEffect.isDefinedAt(op) =>
        interpreter.onEffect(op)(x => reset(interpreter) { k(x) })
      case im: Impure[x, G[R]] =>
        im.copy(continuation = x => reset(interpreter) { im.continuation(x) })
    }


  // EXAMPLES

  case object Get extends Op[Int]
  def get(): Eff[Int] = send(Get)

  case class Put(n: Int) extends Op[Unit]
  def put(n: Int): Eff[Unit] = send(Put(n))


  val prog: Eff[Unit] =
    put(1) andThen put(2) andThen put(3) andThen get() andThen put(3)

  type Id[A] = A
  object PrintPuts extends Interpreter[Id] {
    def onPure[A] = a => pure(a)
    def onEffect[X, R] = {
      case Put(n) => resume => println(n); resume(())
    }
  }

  object Get42 extends Interpreter[Id] {
    def onPure[A] = a => pure(a)
    def onEffect[X, R] = {
      case Get => resume => resume(42)
    }
  }

  println { run { Get42 { PrintPuts { prog } } } }
}

package object idiomUnsafe extends App {

  // for now we don't use union types but accept effect-unsafety

  type Continuation[A, B] = A => Eff[B]
  trait Op[X]

  sealed trait Eff[A] {
    def map[B](f: A => B): Eff[B]
    def flatMap[B](f: A => Eff[B]): Eff[B]
    def ap[B](f: Eff[A => B]): Eff[B] = flatMap { a => f map { _ apply a } }
    def andThen[B](f: Eff[B]): Eff[B] = ap(f map { b => a: A => b })
  }

  sealed trait Idiom[A] extends Eff[A] {
    def map[B](f: A => B): Idiom[B]
    def ap[B](f: Idiom[A => B]): Idiom[B]
    def andThen[B](f: Idiom[B]): Idiom[B] = ap(f map { b => a: A => b })
    def map2[B, C](mb: Idiom[B])(f: (A, B) => C): Idiom[C] = ap(mb.map { b: B => a: A => f(a, b) })
  }

  case class Pure[A](value: A) extends Idiom[A] {
    def map[B](f: A => B): Idiom[B] = Pure(f(value))
    def flatMap[B](f: A => Eff[B]): Eff[B] = f(value)
    def ap[B](ef: Idiom[A => B]): Idiom[B] = ef map { f => f(value) }
  }

  case class ImpureAp[X, A](op: Op[X], continuation: Idiom[X => A]) extends Idiom[A] {
    def map[B](f: A => B): Idiom[B] =
      ImpureAp(op, continuation map { f compose _ })
    def ap[B](ef: Idiom[A => B]): Idiom[B] =
      ImpureAp(op, continuation ap (ef map { (f : A => B) => (g : X => A) => f compose g }))
    def flatMap[B](f: A => Eff[B]): Eff[B] =
      Impure(op, continuation, f)
  }

  // all effect calls start out as idiomatic programs
  def send[X](op: Op[X]): Idiom[X] = ImpureAp[X, X](op, pure(identity))

  case class Impure[X, Y, A](op: Op[X], idiomCont: Idiom[X => Y], monadCont: Continuation[Y, A]) extends Eff[A] {
    def map[B](f: A => B): Eff[B] = Impure(op, idiomCont, y => monadCont(y) map f)
    def flatMap[B](f: A => Eff[B]): Eff[B] = Impure(op, idiomCont, y => monadCont(y) flatMap f)
  }

  def pure[A](a: A): Idiom[A] = Pure(a)
  def run[A](ma: Eff[A]): A = ma match {
    case Pure(a) => a
    case _ => sys error "Cannot run program with unhandled effects: " + ma
  }


  // interpreters / handlers are *partial* functions: Op ~> Domain

  type ~>[-A, +B] = PartialFunction[A, B]

  trait Interpreter[G[_]] {
    // the pure clause is non-effectful for now
    // can be changed to `A => Eff[G[A]]` for monadic and `Idiom[A] => Idiom[G[A]]` for idiomatic handlers
    def onPure[A]: A => G[A]
  }

  // Idiomatic Handlers
  // ==================
  trait Idiomatic[G[_]] extends Interpreter[G] {
    // this is a simplified form of Idiom[G[X => R]] => Idiom[G[R]]
    def map[A, B]: G[A] => (A => B) => G[B]
    def onEffect[X, R]: Op[X] ~> (G[X => R] => G[R])
    def apply[R](prog: Idiom[R]): Idiom[G[R]] = runIdiomatic(this)(prog)
    def dynamic[R](prog: Eff[R])(sequence: Sequencer[G, R]): Eff[R] = runDynamic(this, sequence)(prog)
  }

  def runIdiomatic[R, G[_]](interpreter: Idiomatic[G])(prog: Idiom[R]): Idiom[G[R]] = prog match {
    case p @ Pure(_) =>
      prog map interpreter.onPure
    case ImpureAp(op, k) if interpreter.onEffect isDefinedAt op =>
      interpreter(k) map interpreter.onEffect(op)
    case ImpureAp(op, k) =>
      // here we require G to be a functor to convert `gk: G[x => R]` to `x => G[R]`:
      ImpureAp(op, interpreter(k) map { gk => x => interpreter.map(gk) { xr => xr(x) } })
  }

  // Monadic Handlers
  // ================
  trait Monadic[G[_]] extends Interpreter[G] {
    def onEffect[X, R]: Op[X] ~> (Continuation[X, G[R]] => Eff[G[R]])
    def apply[R](prog: Eff[R]): Eff[G[R]] = runMonadic(this)(prog map onPure)
  }
  def runMonadic[R, G[_]](interpreter: Monadic[G])(prog: Eff[G[R]]): Eff[G[R]] = prog match {
    case p @ Pure(_) =>
      p
    case Impure(op, ki, km) if interpreter.onEffect isDefinedAt op =>
      interpreter.onEffect(op) { x => runMonadic(interpreter) { ki map { _ apply x } flatMap km } }
    case im: Impure[x, y, R] =>
      // Oh! We also need to handle ki with the interpreter! So the following is wrong:
      //    Impure(op, ki, x => runMonadic(interpreter)(km(x)))
      // In this case, we loose the idiomatic structure of our subprogram.
      //
      // the handler is refunctionalized, so we can't simply store it on the stack as a frame...
      println("Forwarding " + im)
      Impure[x, x, R](im.op, pure(identity), x => {
        interpreter { im.idiomCont map { _ apply x } flatMap im.monadCont }
      })
    case ImpureAp(op, k) =>
      runMonadic(interpreter) { Impure(op, k, pure) }
  }

  // Mediating between Idiomatic and Monadic Computation
  // ===================================================

  // Defines how to sequence (embed) idiomatic computation into monadic computation.
  trait Sequencer[G[_], R] {
    def apply[X]: G[X] => Continuation[X, R] => Eff[R]
  }

  def runDynamic[R, G[_]](interpreter: Idiomatic[G], sequence: Sequencer[G, R])(prog: Eff[R]): Eff[R] = prog match {
    case p @ Pure(_) =>
      p
    case Impure(op, ki, km) if interpreter.onEffect isDefinedAt op =>
      println("Dynamic: " + prog)
      for {
        delimited   <- interpreter { ki }
        interpreted <- pure { interpreter.onEffect(op)(delimited) }
        result      <- sequence(interpreted) { x => runDynamic(interpreter, sequence)(km(x)) }
      } yield result

    // TODO this is probably as broken as the monadic forwarding...
    case im: Impure[x, y, R] =>
      Impure[x, x, R](im.op, pure(identity), x => {
        runDynamic(interpreter, sequence) { im.idiomCont map { _ apply x } flatMap im.monadCont }
      })

      // Maybe return
      //     Impure(im.op, im.idiomCont, im.monadCont :+ DynamicHandler(interpreter, sequence) )

//      Impure(op, ki, x => runDynamic(interpreter, sequence)(km(x)))

    // the program is purely idiomatic, no flatMap occurred.
    case ImpureAp(op, k) =>
      runDynamic(interpreter, sequence)(Impure(op, k, pure))
  }

  // Helpers
  // =======
  trait MonadicId extends Monadic[[X] => X] {
    def onPure[A] = a => a
  }

  // Examples
  // ========

  case object Get extends Op[Int]
  def get(): Idiom[Int] = send(Get)

  case class Put(n: Int) extends Op[Unit]
  def put(n: Int): Idiom[Unit] = send(Put(n))


  val prog: Eff[Unit] =
    put(1) andThen put(2) andThen put(3) andThen get() andThen put(4)

  type Id[A] = A
  object PrintPuts extends MonadicId {
    def onEffect[X, R] = {
      case Put(n) => resume => println(n); resume(())
    }
  }

  object Get42 extends MonadicId {
    def onEffect[X, R] = {
      case Get => resume => resume(42)
    }
  }

  object SumPuts extends Idiomatic[[X] => (X, Int)] {
    def onPure[R] = r => (r, 0)
    def map[A, B] = (a, n) => f => (f(a), n)
    def onEffect[X, R] = {
      case Put(n) => (k, m) => (k(()), m + n)
    }
  }

  object CountGets extends Idiomatic[[X] => (X, Int)] {
    def onPure[R] = r => (r, 0)
    def map[A, B] = (a, n) => f => (f(a), n)
    def onEffect[X, R] = {
      case Get => (k, m) => (k(42), m + 1)
    }
  }

  def dynamicGets[R](prog: Eff[R]): Eff[R] =
    CountGets.dynamic(prog)(new Sequencer {
      def apply[X] = (x, n) => resume => resume(x)
    })

  type Trace = List[Int]
  def tracePuts[R](prog: Eff[R]): Eff[(R, Trace)] =
    SumPuts.dynamic(prog map { x => (x, List.empty[Int]) })(new Sequencer {
      def apply[X] = (x, n) => resume => resume(x) map { case (r, ms) => (r, n :: ms) }
    })

  println { run { Get42 { tracePuts { prog } } } }
  println { run { tracePuts { Get42 { prog } } } }

  println { run { dynamicGets { tracePuts { prog } } } }
  println { run { tracePuts { dynamicGets { prog } } } }
}


// In previous implementations, by forwarding idiomatic effects through monadic
// handlers we lost the idiomatic structure of the subprogram.
//
// Idea of this implementation:
//   1) Base implementation only on standard free-monads and free-applicative
//   2) Treat the first `flatMap` on an idiomatic program as a (built-in) effect.
//
// This way we don't need any special casing for "dynamic" handlers. Instead,
// the two types Impure (free monad) and ImpureAp (free applicative) suffice.
//
// To focus on the operation semantics, this implementation is not effect safe.
// establishing static effect safety is a separate (and probably orthogonal)
// step.
package object idiomInject extends App {

  // Type Aliases
  // ============

  type MonadCont[A, B] = A => Eff[B]
  type IdiomCont[A, B] = Idiom[A => B]

  // interpreters / handlers are *partial* natural transformations: Op ~> Domain
  type ~>[-A, +B] = PartialFunction[A, B]


  // Effect Operations
  // =================

  trait Op[X]

  // all effect calls start out as idiomatic programs
  def send[X](op: Op[X]): Idiom[X] = ImpureAp[X, X](op, pure(identity))


  // Base Types
  // ==========

  sealed trait Eff[A] {
    def map[B](f: A => B): Eff[B]
    def flatMap[B](f: A => Eff[B]): Eff[B]
    def ap[B](f: Eff[A => B]): Eff[B] = flatMap { a => f map { _ apply a } }
    def andThen[B](f: Eff[B]): Eff[B] = ap(f map { b => a: A => b })
  }

  sealed trait Idiom[A] extends Eff[A] {
    def map[B](f: A => B): Idiom[B]
    def ap[B](f: Idiom[A => B]): Idiom[B]
    def andThen[B](f: Idiom[B]): Idiom[B] = ap(f map { b => a: A => b })
    def map2[B, C](mb: Idiom[B])(f: (A, B) => C): Idiom[C] = ap(mb.map { b: B => a: A => f(a, b) })
  }

  def pure[A](a: A): Idiom[A] = Pure(a)


  // Bubble Types
  // ============

  case class Pure[A](value: A) extends Idiom[A] {
    def map[B](f: A => B): Idiom[B] = Pure(f(value))
    def flatMap[B](f: A => Eff[B]): Eff[B] = f(value)
    def ap[B](ef: Idiom[A => B]): Idiom[B] = ef map { f => f(value) }
  }

  // this is free applicatives:
  //   https://hackage.haskell.org/package/free-5.1/docs/Control-Applicative-Free.html
  case class ImpureAp[X, A](op: Op[X], continuation: IdiomCont[X, A]) extends Idiom[A] {
    def map[B](f: A => B): Idiom[B] =
      ImpureAp(op, continuation map { f compose _ })
    def ap[B](ef: Idiom[A => B]): Idiom[B] =
      ImpureAp(op, continuation ap (ef map { (f : A => B) => (g : X => A) => f compose g }))
    def flatMap[B](f: A => Eff[B]): Eff[B] =
      Impure(Bind(this), _ flatMap f)
  }

  case class Impure[X, A](op: Op[X], continuation: MonadCont[X, A]) extends Eff[A] {
    def map[B](f: A => B): Eff[B] = Impure(op, x => continuation(x) map f)
    def flatMap[B](f: A => Eff[B]): Eff[B] = Impure(op, x => continuation(x) flatMap f)
  }


  // Interpreters
  // ============

  trait Interpreter[G[_]] {
    // the pure clause is non-effectful for now
    // can be changed to `A => Eff[G[A]]` for monadic and `Idiom[A] => Idiom[G[A]]` for idiomatic handlers
    def onPure[A]: A => G[A]
  }

  // Idiomatic Handlers
  // ------------------

  trait Idiomatic[G[_]] extends Interpreter[G] {
    // this is a simplified form of Idiom[G[X => R]] => Idiom[G[R]]
    def map[A, B]: G[A] => (A => B) => G[B]
    def onEffect[X, R]: Op[X] ~> (G[X => R] => G[R])
    def apply[R](prog: Idiom[R]): Idiom[G[R]] = runIdiomatic(this)(prog)
    def dynamic[R](prog: Eff[R])(sequence: Sequencer[G, R]): Eff[R] = runDynamic(this, sequence)(prog)
  }

  private def runIdiomatic[R, G[_]](interpreter: Idiomatic[G])(prog: Idiom[R]): Idiom[G[R]] = prog match {
    case p @ Pure(_) =>
      prog map interpreter.onPure
    case ImpureAp(op, k) if interpreter.onEffect isDefinedAt op =>
      interpreter(k) map interpreter.onEffect(op)
    case ImpureAp(op, k) =>
      // here we require G to be a functor to convert `gk: G[x => R]` to `x => G[R]`:
      ImpureAp(op, interpreter(k) map { gk => x => interpreter.map(gk) { xr => xr(x) } })
  }

  // Monadic Handlers
  // ----------------

  trait Monadic[G[_]] extends Interpreter[G] {
    def onEffect[X, R]: Op[X] ~> (MonadCont[X, G[R]] => Eff[G[R]])
    def apply[R](prog: Eff[R]): Eff[G[R]] = runMonadic(this)(prog map onPure)
  }
  private def runMonadic[R, G[_]](interpreter: Monadic[G])(prog: Eff[G[R]]): Eff[G[R]] = prog match {
    case p @ Pure(_) =>
      p
    case Impure(op, km) if interpreter.onEffect isDefinedAt op =>
      interpreter.onEffect(op) { x => runMonadic(interpreter) { km(x) } }
    case im: Impure[x, G[R]] =>
      Impure(im.op, x => runMonadic(interpreter) { im.continuation(x) })
    case ImpureAp(op, k) =>
      runMonadic(interpreter) { prog flatMap pure }
  }


  // Idiom Injection
  // ===============
  // the first bind on an idiomatic computation sends a "Bind"-effect.

  // Bind has return type Eff[A] to allow sending effectful values back to the original position. (Time traveling control!)
  private case class Bind[X, A](ap: ImpureAp[X, A]) extends Op[Eff[A]]

  // Default handler for Bind:
  //   case class Bind[X, A](op: Op[X], continuation: Idiom[X => A]) extends Op[Eff[A]]
  private object BindDefault extends MonadicId {
    def onEffect[X, R] = {
      case Bind(ImpureAp(op, k)) => resume => resume(Impure(op, x => k map { _ apply x }))
    }
  }

  def run[A](ma: Eff[A]): A = BindDefault { ma } match {
    case Pure(a) => a
    case _ => sys error "Cannot run program with unhandled effects: " + ma
  }


  // Mediating between Idiomatic and Monadic Computation
  // ---------------------------------------------------

  // Defines how to sequence (embed) idiomatic computation into monadic computation.
  trait Sequencer[G[_], R] {
    def apply[X]: G[X] => (X => Eff[R]) => Eff[R]
  }

  // The combinator `runDynamic` injects the provided interpreter at the position of the
  // first call to flatMap on an idiomatic program. This implies that also all effects
  // used by the interpreter are evaluated at that particular position!
  private def runDynamic[R, G[_]](interpreter: Idiomatic[G], sequence: Sequencer[G, R])(prog: Eff[R]): Eff[R] = new MonadicId { outer =>

    // collects the continuation
    private case class Dynamic[R](gr: G[R]) extends Op[R] { val prompt = outer }

    def onEffect[X, R2] = {
      case Bind(ap @ ImpureAp(op, k)) if interpreter.onEffect.isDefinedAt(op) => resume => {
        // 1) inject idiomatic handler
        val handled = interpreter { ap }

        // Send computation back to the future. It ...
        // 2) runs the idiomatic handler in the original position of the bind
        // 3) collects the continuation (using Dynamic) to be sequenced in a separate step
        resume(handled flatMap (ga => send(Dynamic(ga))))
      }

      case d @ Dynamic(gr) if d.prompt eq this => resume => {
        // this cast is a consequence of interpreters not being fixed to *one particular* answer type.
        val seq = sequence.asInstanceOf[Sequencer[G, R2]]
        seq(gr)(resume)
      }
    }
  } apply prog


  // Helpers
  // =======

  trait MonadicId extends Monadic[[X] => X] {
    def onPure[A] = a => a
  }


  // Cats Interop
  // ============
  import cats.{ Applicative, Functor, Monoid, Monad }
  import cats.implicits._

  implicit object effMonad extends Monad[Eff] {
    override def flatMap[A, B](fa: Eff[A])(f: A => Eff[B]): Eff[B] = fa flatMap f
    override def pure[A](a: A): Eff[A] = idiomInject.pure(a)

    // TODO make tailrecursive
    // @annotation.tailrec
    def tailRecM[A, B](init: A)(fn: A => Eff[Either[A, B]]): Eff[B] =
      fn(init) flatMap {
        case Left(a) => tailRecM(a)(fn)
        case Right(b) => pure(b)
      }
  }

  implicit object idiomApplicative extends Applicative[Idiom] {
    def ap[A, B](ff: Idiom[A => B])(fa: Idiom[A]): Idiom[B] = fa ap ff
    def pure[A](a: A): Idiom[A] = idiomInject.pure(a)
    override def map[A, B](fa: Idiom[A])(f: A => B): Idiom[B] = fa map f
  }

  trait Functorial[F[_]: Functor] extends Idiomatic[F] {
    type G[X] = F[X]
    def map[A, B] = Functor[F].map
  }


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
  object PrintPuts extends MonadicId {
    def onEffect[X, R] = {
      case Put(n) => resume => println(n); resume(())
    }
  }

  // A handler that constantly returns 42
  object Get42 extends MonadicId {
    def onEffect[X, R] = {
      case Get => resume => resume(42)
    }
  }


  type WithInt[X] = (X, Int)

  // An idiomatic handler for put, that statically sums all the puts.
  object SumPuts extends Idiomatic[WithInt] {
    def onPure[R] = r => (r, 0)
    def map[A, B] = (a, n) => f => (f(a), n)
    def onEffect[X, R] = {
      case Put(n) => (k, m) => (k(()), m + n)
    }
  }

  // An idiomatic handler for get, that statically counts all the puts.
  object CountGets extends Idiomatic[WithInt] {
    def onPure[R] = r => (r, 0)
    def map[A, B] = (a, n) => f => (f(a), n)
    def onEffect[X, R] = {
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


  println { run { Get42 { PrintPuts { prog(0) } } } }
  // 1
  // 2
  // 3
  //> 42

  println { run { Get42 { SumPuts { prog(0) } } } }
  //> (42, 6)

  // Doesn't type check since Get42 is a *monadic* handler
  // and we don't have idiom-polymorphism.
  //   println { run { SumPuts { Get42 { prog(0) } } } }

  // however, we can use the tracing variant of puts:
  println { run { Get42 { trace(SumPuts) { prog(0) } } } }
  //> (42, List(6))

  // now also works in this nesting:
  println { run { trace(SumPuts) { Get42 { prog(0) } } } }
  //> (42, List(6))


  // A slightly larger example to illustrate "trace":
  val prog2 = for {
    x <- prog(0)
    y <- prog(1).map2(get()) { _ + _ }
    z <- prog(2)
  } yield (x, y, z)

  println { run { Get42 { trace(SumPuts) { prog2 } } } }
  //> ((42,42,42), List(6, 9, 12))

  println { run { trace(CountGets) { trace(SumPuts) { prog2 } } } }
  //> (((0,0,0), List(6, 9, 12)), List(1, 2, 1))
  //    ^^^^^^     ^^^^^^^^^^     ^^^^^^^^^^^^
  //  the result  the put trace   the get trace


  // A handler for tick, that calls the continuation twice
  object AmbiguousTick extends Monadic[List] {
    def onPure[R] = a => List(a)
    def onEffect[X, R] = {
      case Tick => resume => {
        println("tick")
        for {
          first <- resume(())
          second <- resume(())
        } yield first ++ second
      }
    }
  }

  println { run { AmbiguousTick { tick() }}}
  //> List((), ())

  println { run { AmbiguousTick { trace(SumPuts) {
    tick() andThen put(2)
  }}}}
  //> List(((),List(2)), ((),List(2)))

  println { run { trace(SumPuts) { AmbiguousTick {
    tick() andThen put(2)
  }}}}
  //> (List((), ()), List(2, 2))

  // A larger program with nested idiomatic computation
  val prog3: Eff[(Int, Int, Int)] = for {
    x <- prog(0)
    y <- put(1337) andThen (for {
      a <- tick() andThen List(10,20,30).traverse(prog) andThen get()
      b <- tick() andThen prog(2)
    } yield a + b)
    z <- get() andThen prog(3)
  } yield (x, y, z)

  println("--------------------")
  println { run { AmbiguousTick { Get42 { tracePuts { prog3 } } } } }
  //> List(((42,84,42),List(6, 1337, 198, 12, 15)),
  //       ((42,84,42),List(6, 1337, 198, 12, 15)),
  //       ((42,84,42),List(6, 1337, 198, 12, 15)),
  //       ((42,84,42),List(6, 1337, 198, 12, 15)))


//  println("--------------------")
//  println { run { Get42 { AmbiguousTick { tracePuts { prog3 } } } } }
//  println("--------------------")
//  println { run {  tracePuts { Get42 { AmbiguousTick { prog3 } } } } }
}
package effekt
package examples

import utils._

object parsing extends App {

  trait Fail extends Eff {
    def fail(msg: String = ""): Nothing / effect
  }


  trait Choose extends Eff {
    def chooseBetween[A](first: A, second: A): A / effect

    def choose[A](first: A, other: A*): A / effect =
      if (other.isEmpty) pure(first)
      else for {
        x <- chooseBetween(first, other.head)
        r <- choose(x, other.tail : _*)
      } yield r

    def chooseEffect[A, E](first: A / E, other: (A / E)*): A / (effect & E) =
      choose(first, other: _*).flatMap { identity }
  }

  trait Input[S] extends Eff {
    def read(): S / effect
  }
  def Input[S] given (cap: Input[S]): cap.type = cap

  trait Cache extends Eff {
    def cacheValue[A](p: => A): A / effect
    def cache[A, E](p: => A / E): A / (effect & E) =
      cacheValue[A / E](p).flatMap { identity }
  }

  type Token = String
  trait Parser[S] extends Fail with Choose with Cache with Input[S] {
    def expect(t: S): S / effect = for {
      s   <- read()
      res <- if (s == t) pure(s) else fail(s"expected $t but got $s")
    } yield res
  }
  type P = Parser[Token]
  def P given (p: P): p.type = p


  implicit def token(s: Token) given (p: P): Token / p.effect = P.expect(s)

  trait TokenOps {
      def (t: Token) <~[FX]    (parser: Any / FX) given P : Token / (P.effect & FX)      = token(t) <~ parser
      def (t: Token) ~>[B, FX] (parser: B / FX)   given P : B / (P.effect & FX)          = token(t) ~> parser
      def (t: Token) ~ [A, FX] (parser2: A / FX)  given P : (Token, A) / (P.effect & FX) = token(t) ~ parser2
      def (t: Token) ^^[B]     (f: Token => B)    given P : B / P.effect                 = token(t).map(f)
      def (t: Token) ^^^[B]    (result: B)        given P : B / P.effect                 = token(t).map(x => result)
  }
  delegate for TokenOps

  trait ControlOps[A, FX] {
    def (c1: A / FX) <~ [FX2]    (c2: Any / FX2) = for { x1 <- c1; _ <- c2 } yield x1
    def (c1: A / FX) ~> [B, FX2] (c2: B / FX2)   = for { _ <- c1; x2 <- c2 } yield x2
    def (c1: A / FX) ~  [B, FX2] (c2: B / FX2)   = for { x1 <- c1; x2 <- c2 } yield (x1, x2)
    def (c1: A / FX) ^^ [B]      (f: A => B)     = c1.map(f)
    def (c1: A / FX) ^^^[B]      (result: B)     = c1.map(x => result)
  }
  delegate [A, FX] for ControlOps[A, FX]

  trait ParserOps[A, FX] {
    def (p1: A / FX) | [B >: A, FX2] (p2: B / FX2) given P = P.chooseEffect(p1, p2)
  }
  delegate [A, FX] for ParserOps[A, FX]

  def AB given P : Int / P.effect =
    P.cache ( log("try A") ~> "A" ~> AB ^^  { _ + 1 }
            | log("try B") ~> "B"       ^^^ { 0 }
            )

  def ambiguous given P: Int / P.effect =
    P.cache ( log("try A1") ~> "A" ~> ambiguous ^^  { _ + 1 }
            | log("try A2") ~> "A" ~> "B"       ^^^  { 42 }
            | log("try B") ~> "B"               ^^^ { 0 }
            )


  trait MyParsers {
    implicit val P: P

    lazy val AB: Int / P.effect =
      P.cache ( "A" ~> AB ^^  { _ + 1 }
              | "B"       ^^^ { 0 }
              )
  }

  // Using the state monad interpretation, since it also allows thunking of recursive nonterminals.
  class RecursiveDecent[R, FX](val input: Seq[Token]) extends Parser[Token] with StatefulHandler[R, Option[R]] {

    type effects = FX

    val pos = Field(0)

    def unit = r => pure(Some(r))

    def fail(msg: String = "") = use { pure(None) }
    def read() = use { for {
      cur <- pos.value
      tok = input(cur)
      _   <- pos.value = cur + 1
      res <- resume(tok)
    } yield res }

    def cacheValue[A](p: => A) = use { resume(p) }
    def chooseBetween[A](first: A, second: A) = use { for {
      before <- pos.value
      r   <- resume(first)
      res <- if (r.isDefined) pure(r) else (pos.value = before) andThen resume(second)
    } yield res }
  }

  locally {
    def parse(in: Token*) = println { run { new RecursiveDecent(in) handle { AB } } }

    parse("B")
    parse("A", "A", "B")
    parse("A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "B")
  }

  import schedulerState.Process

  trait Out[R] extends Eff {
    def write(value: R): Unit / effect
  }
  def Out[R] given (cap: Out[R]) : cap.type = cap

  class FirstOnly[R, FX] extends Out[R] with Handler[Unit, Option[R]] {
    type effects = FX
    def unit = r => pure(None)
    def write(value: R): Unit / effect = use { pure(Some(value)) }
  }

  class All[R, FX] extends Out[R] with Handler[Unit, List[R]] {
    type effects = FX
    def unit = r => pure(Nil)
    def write(value: R): Unit / effect = use { resume(()) map { xs => value :: xs }}
  }

  class Reader[T, R, FX](val input: Seq[T]) extends Input[T] with StatefulHandler[R, R] {
    type effects = FX
    val pos = Field(0)
    def unit = r => pure(r)
    def read(): T / effect = use { for {
      cur <- pos.value
      _   <- pos.value = cur + 1
      res <- resume(input(cur))
    } yield res }
  }


  // Reusing the existing scheduler.

  class BreadthFirst[FX, P <: Process, I <: Input[Token]] given (val p: P) given (val in: I)
      extends Parser[Token] with Handler[Unit, Unit] {

    type effects = p.effect & in.effect & FX

    def unit = r => pure(())

    def fail(msg: String = "") = use { pure(()) }

    // AB ::= 'A' AB | 'B'
    // AB = nonterminal('A' ~ AB | 'B')

    // nonterminals are like yield-points
    def cacheValue[A](a: => A) = use {
      Process.suspend() flatMap { _ => resume(a) }
    }

    // just forwarding to fork
    def chooseBetween[A](first: A, second: A) = use { for {
      forked <- Process.fork()
      result <- if (forked) resume(first) else resume(second)
    } yield result }

    def read(): Token / effect = use { Input.read() flatMap resume }
  }

  import schedulerState.Scheduler

  def ParserFirst[R, FX](parser: given (p: P) => R / (p.effect & FX)): Seq[Token] => Option[R] / FX = in =>
    new FirstOnly handle {      // [R, FX]
      new Scheduler handle {    // [FX & Out.effect]
        new Reader(in) handle { // [Token, Unit, FX & Out.effect & Process.effect]
          new BreadthFirst[FX & Out.effect, Process.type, Input.type] handle {
            parser flatMap { r => Out.write(r) }
          }
        }
      }
    }

  def ParserAll[R, FX](parser: given (p: P) => R / (p.effect & FX)): Seq[Token] => List[R] / FX = in =>
    new All handle {            // [R, FX]
      new Scheduler handle {    // [FX & Out.effect]
        new Reader(in) handle { // [Token, Unit, FX & Out.effect & Process.effect]
          new BreadthFirst[FX & Out.effect, Process.type, Input.type] handle {
            parser flatMap { r => Out.write(r) }
          }
        }
      }
    }

  locally {
    val p = ParserAll { ambiguous }

    def test(in: Token*) = println { run { p(in: _*) } }
    // crashes currently since our Reader implementation is unsafe.
    // test()
    test("B")
    test("A", "A", "B")
    test("A", "A", "B", "C")
    test("A", "A", "C", "B")
    test("A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "B")
  }
}

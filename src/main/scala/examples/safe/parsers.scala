package examples.safe

import effekt._
import utils._

object parsers extends App {

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


  // To avoid capability passing, we can also refer to stable fields:
  trait MyParsers {
    implicit val P: P

    lazy val AB: Int / P.effect =
      P.cache ( "A" ~> AB ^^  { _ + 1 }
              | "B"       ^^^ { 0 }
              )
  }

  // Using the state monad interpretation, since it also allows thunking of recursive nonterminals.
  trait RecursiveDecent[R, E](val input: Seq[Token]) extends Parser[Token] {

    val state: State
    val scope: Scope[Option[R], state.effect & E]
    type effect = scope.effect

    lazy val pos = state.Field(0)

    def fail(msg: String = "") = scope { pure(None) }
    def read() = scope { for {
      cur <- pos.value
      tok = input(cur)
      _   <- pos.value = cur + 1
      res <- resume(tok)
    } yield res }

    def cacheValue[A](p: => A) = scope { resume(p) }
    def chooseBetween[A](first: A, second: A) = scope { for {
      before <- pos.value
      r   <- resume(first)
      res <-
        if (r.isDefined) pure(r)
        else { pos.value = before } andThen resume(second)
    } yield res }
  }
  def recursiveDecent[R, E](in: Seq[Token])(prog: given (p: P) => R / (p.effect & E)): Option[R] / E =
    region { handle[Option[R], State.effect & E] {
        prog given new RecursiveDecent[R, E](in) {
          val state: State.type = State; val scope: Scope.type = Scope
        } map { r => Some(r) }
      }
    }

  locally {
    def parse(in: Token*) = println { run { recursiveDecent(in) { AB } } }

    parse("B")
    parse("A", "A", "B")
    parse("A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "B")
  }

  trait Out[R] extends Eff {
    def write(value: R): Unit / effect
  }
  def Out[R] given (cap: Out[R]) : cap.type = cap

  def firstOnly[R, E](prog: given (o: Out[R]) => Unit / (o.effect & E)) =
    handle [Option[R], E] {
      prog given new Out[R] {
        type effect = Scope.effect
        def write(value: R) = scope { pure(Some(value)) }
      } map { _ => None }
    }

  def all[R, E](prog: given (o: Out[R]) => Unit / (o.effect & E)) =
    handle [List[R], E] {
      prog given new Out[R] {
        type effect = Scope.effect
        def write(value: R) = scope { resume(()) map { xs => value :: xs } }
      } map { _ => Nil }
    }

  def reader[T, R, E](input: Seq[T])(prog: given (in: Input[T]) => R / (in.effect & E)): R / E =
    region {
      prog given new Input[T] {
        type effect = State.effect
        val pos = Field(0)
        def read() = for {
          cur <- pos.value
          _   <- pos.value = cur + 1
        } yield input(cur)
      }
    }

  // Reusing the existing scheduler.
  import scheduler._

  class BreadthFirst[F <: Fiber, I <: Input[Token]] given (val f: F) given (val in: I) extends Parser[Token] {
    type effect = f.effect & in.effect

    def fail(msg: String = "") = Fiber.exit()
    // nonterminals are like yield-points
    def cacheValue[A](a: => A) =
      Fiber.suspend() map { _ => a }

    // just forwarding to fork
    def chooseBetween[A](first: A, second: A) =
      Fiber.fork() map { forked => if (forked) first else second }

    def read(): Token / effect = Input.read()
  }

  def parseFirst[R, E](parser: given (p: P) => R / (p.effect & E)): Seq[Token] => Option[R] / E = in =>
    firstOnly {
      schedule[E & Out.effect] {
        reader[Token, Unit, E & Out.effect & Fiber.effect](in) {
          parser given new BreadthFirst[Fiber.type, Input.type] flatMap { Out.write(_) }
        }
      }
    }
  def parseAll[R, E](parser: given (p: P) => R / (p.effect & E)): Seq[Token] => List[R] / E = in =>
    all {
      schedule[E & Out.effect] {
        reader[Token, Unit, E & Out.effect & Fiber.effect](in) {
          parser given new BreadthFirst[Fiber.type, Input.type] flatMap { Out.write(_) }
        }
      }
    }

  locally {
    val p = parseAll { ambiguous }

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

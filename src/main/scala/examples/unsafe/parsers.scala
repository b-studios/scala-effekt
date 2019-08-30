package examples.unsafe

import effekt.unsafe._
import utils._

object parsers extends App {

  trait Fail {
    def fail(msg: String = ""): Control[Nothing]
  }

  trait Choose {
    def chooseBetween[A](first: A, second: A): Control[A]

    def choose[A](first: A, other: A*): Control[A] =
      if (other.isEmpty) pure(first)
      else for {
        x <- chooseBetween(first, other.head)
        r <- choose(x, other.tail : _*)
      } yield r

    def chooseEffect[A](first: Control[A], other: Control[A]*): Control[A] =
      choose(first, other: _*).flatMap { identity }
  }

  trait Input[S] {
    def read(): Control[S]
  }
  def Input[S] given (cap: Input[S]): cap.type = cap

  trait Cache {
    def cacheValue[A](p: => A): Control[A]
    def cache[A, E](p: => Control[A]): Control[A] =
      cacheValue[Control[A]](p).flatMap { identity }
  }

  type Token = String
  trait Parser[S] extends Fail with Choose with Cache with Input[S] {
    def expect(t: S): Control[S] = for {
      s   <- read()
      res <- if (s == t) pure(s) else fail(s"expected $t but got $s")
    } yield res
  }
  type P = Parser[Token]
  def P given (p: P): p.type = p


  implicit def token(s: Token) given (p: P): Control[Token] = P.expect(s)

  trait TokenOps {
      def (t: Token) <~    (parser: Control[Any]) given P : Control[Token]      = token(t) <~ parser
      def (t: Token) ~>[B] (parser: Control[B])   given P : Control[B]          = token(t) ~> parser
      def (t: Token) ~ [A] (parser2: Control[A])  given P : Control[(Token, A)] = token(t) ~ parser2
      def (t: Token) ^^[B] (f: Token => B)        given P : Control[B]          = token(t).map(f)
      def (t: Token) ^^^[B](result: B)            given P : Control[B]          = token(t).map(x => result)
  }
  delegate for TokenOps

  trait ControlOps[A] {
    def (c1: Control[A]) <~     (c2: Control[Any]) = for { x1 <- c1; _ <- c2 } yield x1
    def (c1: Control[A]) ~> [B] (c2: Control[B])   = for { _ <- c1; x2 <- c2 } yield x2
    def (c1: Control[A]) ~  [B] (c2: Control[B])   = for { x1 <- c1; x2 <- c2 } yield (x1, x2)
    def (c1: Control[A]) ^^ [B] (f: A => B)        = c1.map(f)
    def (c1: Control[A]) ^^^[B] (result: B)        = c1.map(x => result)
  }
  delegate [A] for ControlOps[A]

  trait ParserOps[A] {
    def (p1: Control[A]) | [B >: A] (p2: Control[B]) given P = P.chooseEffect(p1, p2)
  }
  delegate [A] for ParserOps[A]

  def AB given P : Control[Int] =
    P.cache ( log("try A") ~> "A" ~> AB ^^  { _ + 1 }
            | log("try B") ~> "B"       ^^^ { 0 }
            )

  def ambiguous given P: Control[Int] =
    P.cache ( log("try A1") ~> "A" ~> ambiguous ^^  { _ + 1 }
            | log("try A2") ~> "A" ~> "B"       ^^^  { 42 }
            | log("try B") ~> "B"               ^^^ { 0 }
            )


  // To avoid capability passing, we can also refer to stable fields:
  trait MyParsers {
    implicit val P: P

    lazy val AB: Control[Int] =
      P.cache ( "A" ~> AB ^^  { _ + 1 }
              | "B"       ^^^ { 0 }
              )
  }

  // Using the state monad interpretation, since it also allows thunking of recursive nonterminals.
  trait RecursiveDecent[R](val input: Seq[Token]) given State given Scope[Option[R]] extends Parser[Token] {

    private lazy val pos = Field(0)

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
  def recursiveDecent[R, E](in: Seq[Token])(prog: given P => Control[R]): Control[Option[R]]=
    region { handle { prog given new RecursiveDecent(in) {} map { r => Some(r) } } }

  locally {
    def parse(in: Token*) = println { run { recursiveDecent(in) { AB } } }

    parse("B")
    parse("A", "A", "B")
    parse("A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "B")
  }

  trait Out[R] {
    def write(value: R): Control[Unit]
  }
  def Out[R] given (cap: Out[R]) : cap.type = cap

  // here we are using a SAM lambda
  def firstOnly[R](prog: given Out[R] => Control[Unit]): Control[Option[R]] = handle {
    prog given { value => scope { pure(Some(value)) } } map { _ => None }
  }

  def all[R](prog: given Out[R] => Control[Unit]): Control[List[R]] = handle {
    prog given { value => scope { resume(()) map { xs => value :: xs } } } map { _ => Nil }
  }

  def reader[T, R, E](input: Seq[T])(prog: given Input[T] => Control[R]): Control[R] =
    region {
      val pos = Field(0)
      prog given { () => for {
        cur <- pos.value
        _   <- pos.value = cur + 1
      } yield input(cur) }
    }

  // Reusing the existing scheduler.
  import scheduler._

  class BreadthFirst given Fiber given Input[Token] extends Parser[Token] {
    def fail(msg: String = "") = Fiber.exit()
    // nonterminals are like yield-points
    def cacheValue[A](a: => A) =
      Fiber.suspend() map { _ => a }

    // just forwarding to fork
    def chooseBetween[A](first: A, second: A) =
      Fiber.fork() map { forked => if (forked) first else second }

    def read() = Input.read()
  }

  def parseFirst[R](parser: given P => Control[R]): Seq[Token] => Control[Option[R]] = in =>
    firstOnly {
      schedule {
        reader(in) {
          parser given new BreadthFirst flatMap { Out.write(_) }
        }
      }
    }
  def parseAll[R](parser: given P => Control[R]): Seq[Token] => Control[List[R]] = in =>
    all {
      schedule {
        reader(in) {
          parser given new BreadthFirst flatMap { Out.write(_) }
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

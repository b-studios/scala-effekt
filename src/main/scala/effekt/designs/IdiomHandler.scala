package effekt.design.idioms

// for rank-2 types
// not really safe, since values of type can
// be stored in references and recovered later.
object rank2 {
  type ω
}
import rank2.ω

// TODO just use cats for real
//object cats {
//
//  implicit def setMonoid[A]: Monoid[Set[A]] = new Monoid[Set[A]] {
//    def unit = Set.empty
//    def plus(fst: Set[A], snd: Set[A]) = fst ++ snd
//  }
//
//  trait Monoid[A] {
//    def unit: A
//    def plus(fst: A, snd: A): A
//  }
//
//  trait Pointed[F[_]] {
//    def unit[R]: R => F[R]
//    def map[A, B](fa: F[A])(f: A => B): F[B]
//  }
//
//}

object effekt {
  import cats.{ Applicative, Monoid, Monad }

  // LIBRARY

  type using[A, E] = implicit E => A
  type and[A, E] = implicit E => A


  type C[A] = Control[A]

  sealed trait Control[+A] {
    def run: A
    def map[B](f: A => B): C[B]
    def flatMap[B](f: A => C[B]): C[B]
    def map2[B, D](mb: C[B])(f: (A, B) => D): C[D] = for {
      a <- this
      b <- mb
    } yield f(a, b)

    def ap[B](mf: C[A => B]): C[B] = map2(mf) { (a, f) => f(a) }
    def andThen[B](mb: C[B]): C[B] = map2(mb) { (a, b) => b }
    def *>[B](mb: C[B]): C[B] = andThen(mb)
  }

  object Control {
    implicit object controlMonad extends Monad[Control] {
      override def flatMap[A, B](fa: C[A])(f: A => C[B]): C[B] = fa flatMap f
      override def pure[A](a: A): C[A] = effekt.pure(a)

      // TODO make tailrecursive
      // @annotation.tailrec
      def tailRecM[A, B](init: A)(fn: A => C[Either[A, B]]): C[B] =
        fn(init) flatMap {
          case Left(a) => tailRecM(a)(fn)
          case Right(b) => pure(b)
        }
      }
  }

  type I[+A] = Idiom[A]

  // idiomatic control, without flatMap
  sealed trait Idiom[+A] extends Control[A] {
    def run: A
    def map[B](f: A => B): I[B]
    def map2[B, C](mb: I[B])(f: (A, B) => C): I[C]

    def ap[B](mf: I[A => B]): I[B] = map2(mf) { (a, f) => f(a) }
    def andThen[B](mb: I[B]): I[B] = map2(mb) { (a, b) => b }
    def *>[B](mb: I[B]): I[B] = andThen(mb)
  }

  object Idiom {
    implicit object idiomApplicative extends Applicative[Idiom] {
      def ap[A, B](ff: I[A => B])(fa: I[A]): I[B] = fa ap ff
      def pure[A](a: A): I[A] = effekt.pure(a)
      override def map[A, B](fa: I[A])(f: A => B): I[B] = fa map f
    }
  }

  def traverse[A, B](l: List[A])(f: A => I[B]): I[List[B]] = l match {
    case Nil => pure(Nil)
    case a :: as => f(a).map2(traverse(as)(f))(_ :: _)
  }

  case class Pure[A](a: A) extends I[A] {
    def run = a
    override def map[B](f: A => B): I[B] = pure(f(a))
    override def map2[B, C](mb: I[B])(f: (A, B) => C): I[C] = mb.map { b => f(a, b) }
    def flatMap[B](f: A => C[B]): C[B] = f(a)
  }


    // The stack is assumed to have the shape:
    //
    // |       |             |
    // +-------+-------------|
    // |  op() |      k1     |  idiomatic fragment of the stack (UseI)  \
    // |  ...  | map2(...)   |                                          |
    // +-------+-------------+                                           > UseD
    // |      flatMap(f1)    |                                          |
    // |      flatMap(f2)    |  monadic fragment of the stack   (UseM)  /
    // |        ...          |
    // +---------------------+
    // |    handleDynamic()  |  the handler (dynamic handler with access to both fragments)


  // TODO add another continuation fragement: km: Option[X => C[A]]
  case class UseI[A, X](h: handler.Idiomatic, body: h.CPS[X], ki: I[X => A]) extends I[A] {
    def run = sys error "undelimited idiom"
    def map[B](f: A => B): I[B] =
      UseI(h, body, ki map { _ andThen f })
    def map2[B, C](mb: I[B])(f: (A, B) => C): I[C] =
      UseI(h, body, ki.map2(mb) { (xa, b) => x => f(xa(x), b) })

    // Here we need to idiomatically reset the idiomatic continuation...
    // This is similar to installing a separate, idiomatic handler that
    // communicates with the outer (monadic) handler via `run`.
    def flatMap[B](f: A => C[B]): C[B] =
    {
      h match {
        case hd: handler.Dynamic[r] =>
          // a flatMap acts like a reset for the inner (idiomatic) handler
          val ga: I[h.G[A]] = h.CPS[X, A](body)(reset(h) { ki })
          // now it is up to the outer (monadic) handler
          UseD(hd, ga.asInstanceOf[I[hd.G[A]]], f)
        case _ => sys error "should not happen! control computation within idiomatic handler."
      }
    }
  }

  case class UseD[A, X](h: handler.Dynamic[_], ki: I[h.G[X]], km: X => C[A]) extends C[A] {
    def run = sys error "undelimited idiom"
    def map[B](f: A => B): C[B] = UseD(h, ki, a => km(a) map f)
    def flatMap[B](f: A => C[B]): C[B] = UseD(h, ki, a => km(a) flatMap f)
  }

  // TODO here UseM extends I since the effect signatures promise idiomatic effects
  // It should be C[A] after the first call to flatMap! Otherwise the signature of `map` is a lie.
  case class UseM[X, A](h: handler.Monadic[_], body: h.CPS[X], km: X => C[A]) extends I[A] {
    def run = sys error "undelimited control"
    def map[B](f: A => B): I[B] = UseM(h, body, a => km(a) map f)
    def flatMap[B](f: A => C[B]): C[B] = UseM(h, body, a => km(a) flatMap f)

    // TODO is this correctly mixing idiomatic and monadic?
    def map2[B, D](mb: I[B])(f: (A, B) => D): I[D] =
      UseM(h, body, a => km(a).map2(mb)(f))
  }

  def pure[A](value: => A): I[A] = new Pure(value)
  def run[A](i: C[A]): A = i.run
  def map3[A, B, C, R](ia: I[A], ib: I[B], ic: I[C])(f: (A, B, C) => R): I[R] =
    ia.map2(ib) { case (a, b) => (a, b) }.map2(ic) { case ((a, b), c) => f (a, b, c) }
  def map4[A, B, C, D, R](ia: I[A], ib: I[B], ic: I[C], id: I[D])(f: (A, B, C, D) => R): I[R] =
    map3(ia, ib, ic) { case (a, b, c) => (a, b, c) }.map2(id) { case ((a, b, c), d) => f (a, b, c, d) }


  // We have three flavors of handlers:
  // 1) idiomatic handlers: I[R] => I[G[R]]
  // 2) monadic handlers:   C[R] => C[R]
  // 3) dynamic handlers:   C[R] => C[G[R]] (but with idiomatic opt.)
  object handler {

    // Builtin handlers

    trait Idiomatic {
      type G[_]
      type CPS[A] = I[G[A => ω]] => I[G[ω]]

      // G is pointed (would be equiv to providing G[Unit])
      def unit[R]: R => G[R]

      // G needs to be a functor.
      def map[A, B]: (A => B) => G[A] => G[B]

      // ideally use would be parametric in R (with a rank-2 type).
      def useEff[A](body: CPS[A]): I[A] = UseI(this, body, pure(a => a))

      // useEff is a bit more expressive, but this often suffices
      def use[A](body: G[A => ω] => G[ω]): I[A] = useEff { _ map body }

      def handle[R](prog: this.type => I[R]): I[G[R]] = effekt.handle(this)(prog)
      def apply[R](prog: this.type => I[R]): I[G[R]] = effekt.handle(this)(prog)

      private[effekt] def CPS[A, R](body: CPS[A])(g: I[G[A => R]]): I[G[R]] =
        body.asInstanceOf[I[G[A => R]] => I[G[R]]](g)
    }

    trait Analyze[D] extends Monoidal[D] {
      def default[R] = use { identity }
      def collect[A](el: D): I[A] = use { d => m.combine(el, d) }
    }

    // a handler for monadic programs that allows dynamic optimizations
    // for idiomatic subprograms.
    trait Dynamic[R] extends Idiomatic {
      def run[X]: G[X] => (X => C[R]) => C[R]

      def handleDynamic(prog: this.type => C[R]): C[R] = effekt.handle(this)(prog)
    }

    // a handler for monadic programs that is itself monadic normal bubble semantics
    trait Monadic[R] {
      type CPS[A] = (A => C[R]) => C[R]
      def use[A](body: CPS[A]): I[A] = UseM(this, body, pure)

      def handle(prog: this.type => C[R]): C[R] = effekt.handle(this)(prog)
      def apply(prog: this.type => C[R]): C[R] = effekt.handle(this)(prog)
    }


    // Some other derived handlers

    trait Monoidal[D](implicit val m: Monoid[D]) extends Idiomatic {
      type G[X] = D
      def unit[R] = r => m.empty
      def map[A, B] = f => d => d
    }

    trait Id extends Idiomatic {
      type G[X] = X
      def unit[R] = identity
      def map[A, B] = _.apply
    }
  }


  def handle[R](h: handler.Idiomatic)(prog: h.type => I[R]): I[h.G[R]] = reset(h) { prog(h) }
  private[effekt] def reset[R](h: handler.Idiomatic)(prog: I[R]): I[h.G[R]] = prog match {
    case p @ Pure(_) =>
      p.map(h.unit)

    // since h eq u.h
    // we can assume that h.G =:= u.h.G
    case u : UseI[R, x] if h eq u.h =>
      u.h.CPS(u.body)(reset(u.h) { u.ki }).asInstanceOf[I[h.G[R]]]

    case u : UseI[R, x] =>
      val k: I[x => h.G[R]] = reset(h)(u.ki) map { gk => x =>
        // here we require G to be a functor to convert `gk: G[x => R]` to `x => G[R]`:
        h.map[x => R, R](xr => xr(x))(gk)
      }
      UseI(u.h, u.body, k)

    // interaction between outer monadic handlers and idiomatic programs:
    //   handleMonad { m => handleIdiom { i => idiomProg(... m.op() ...) }}
    case u : UseM[a, x] =>
      // FIXME doesn't typecheck, since the continuation is not idiomatic. So we can't reset it
      //       with this reset. Hence the cast.
      // this case is only necessary since we declared UseM <: I
      // we could have a separate bubble type for UseMI that is an idiomatic prog.
      // This way we could avoid the cast.
      UseM(u.h, u.body, x => reset(h) { u.km(x).asInstanceOf[I[R]] })
  }

  def handle[R](h: handler.Dynamic[R])(prog: h.type => C[R]): C[R] = reset(h) { prog(h) }
  private[effekt] def reset[R](h: handler.Dynamic[R])(prog: C[R]): C[R] = prog match {
    case p : Pure[R] => p

    // the program is purely idiomatic, no flatMap occurred.
    case u : UseI[R, x] if h eq u.h =>
      reset(h) { u flatMap { pure } }

    case u : UseD[R, x] if h eq u.h =>
      val ki: I[u.h.G[x]] = u.ki
      ki.asInstanceOf[I[h.G[x]]] flatMap { gx =>
        h.run(gx)(x => reset(h) { u.km(x) })
      }

    case u : UseM[x, a] =>
      UseM(u.h, u.body, x => reset(h) { u.km(x) })

    case u : UseD[r, x] =>
      UseD(u.h, u.ki, x => reset(h) { u.km(x) })

    // since this handler is monadic, there shouldn't be another unhandled idiomatic effect
    case u : UseI[R, x] =>
      sys error "should not happen! Unhandled idiomatic effect"
  }

  def handle[R](h: handler.Monadic[R])(prog: h.type => C[R]): C[R] = reset(h) { prog(h) }
  private[effekt] def reset[R](h: handler.Monadic[R])(prog: C[R]): C[R] = prog match {
    case p: Pure[R] => p

    case u : UseM[x, r] if h eq u.h =>
      u.body.asInstanceOf[(x => C[R]) => C[R]](x => reset(h) { u.km(x) })

    case u : UseM[x, a] =>
      UseM(u.h, u.body, x => reset(h) { u.km(x) })

    case u: UseD[a, x] =>
      UseD(u.h, u.ki, x => reset(h) { u.km(x) })

    // Can only occur through handleIdiom { i => handleMonadic { m => i.op() } }
    // but this should be ruled out by `handleMonadic: C[R]` and `handleIdiom(I[R])`.
    case i : UseI[r, x] =>
      sys error "should not happen! Unhandled idiomatic effect"
  }
}

object examples extends App {

  import effekt._
  import handler._

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

  trait Amb { def choose[A](fst: I[A], snd: I[A]): I[A] }
  def Amb: Amb using Amb = implicit p => p

  val example: I[String] using Tick and Put = {
    Tick.tick() andThen Put.put(3) andThen Tick.tick() andThen Put.put(7) andThen pure("done")
  }

  val exampleGet: I[Int] using Get and Put = {
    map3(Get.get(), Get.get(), Put.put(4) andThen Put.put(7)) {
      case (x, y, _) => x + y
    }
  }

  class CountTicks extends Tick with Idiomatic {
    type G[X] = (X, Int)
    def unit[R] = r => (r, 0)
    def map[A, B] = f => (a, n) => (f(a), n)
    def tick() = use { case (k, n) => (k(()), n + 1) }
  }
  def countTicks = new CountTicks

  class SumPuts extends Put with Idiomatic {
    type G[X] = (X, Int)
    def unit[R] = r => (r, 0)
    def map[A, B] = f => (a, n) => (f(a), n)
    def put(n: Int) = use { case (k, m) => (k(()), m + n) }
  }
  def sumPuts = new SumPuts

  class OnlySum extends Put with Idiomatic {
    type G[X] = Int
    def unit[R] = r => 0
    def map[A, B] = f => n => n
    def put(n: Int) = use { _ + n }
  }
  def onlySum = new OnlySum

  class CountGet extends Get with Idiomatic {
    type G[X] = Int
    def unit[R] = r => 0
    def map[A, B] = f => n => n
    def get() = use { _ + 1 }
  }
  def countGet = new CountGet

  class CountGet2 extends Get with Idiomatic {
    type G[R] = (R, Int)
    def unit[R] = r => (r, 0)
    def map[A, B] = f => (a, n) => (f(a), n)
    def get() = use { case (k, n) => (k(0), n + 1) }
  }
  def countGet2 = new CountGet2

  class Both extends Put with Get with Idiomatic {
    type G[X] = (X, Int)
    def unit[R] = r => (r, 0)
    def map[A, B] = f => (a, n) => (f(a), n)
    def get() = use { case (k, m) => (k(m), m) }
    def put(n: Int) = use { case (k, m) => (k(()), n) }
  }
  def both = new Both

  class MonadicGet[R] extends Get with Monadic[List[R]] {
    def get() = use { resume =>
      for {
        xs <- resume(0)
        ys <- resume(1)
      } yield xs ++ ys
    }
  }
  def monadicGet[R](prog: C[R] using Get): C[List[R]] =
    new MonadicGet[R] handle { g => prog(g) map { x => List(x) } }

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
  class TracePuts[R] extends Put with Dynamic[(R, Trace)] {
    // the applicative carrier
    type G[X] = (X, Int)
    def unit[R] = r => (r, 0)
    def map[A, B] = f => (a, n) => (f(a), n)

    // the applicative operation
    def put(n: Int) = use { case (k, m) => (k(()), m + n) }

    // connection to the monadic carrier
    def run[X] = (x, n) => resume => resume(x) map {
      case (r, ms) => (r, n :: ms)
    }
  }
  def tracePuts[R](prog: C[R] using Put): C[(R, Trace)] =
    new TracePuts[R] handleDynamic  { g => prog(g) map { x => (x, Nil) } }

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
}
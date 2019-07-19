import scala.annotation.tailrec

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
package object effekt {


  // Type Aliases
  // ============

  // interpreters / handlers are *partial* natural transformations: Op ~> Domain
  type ~>[-A, +B] = PartialFunction[A, B]


  // Effect Operations
  // =================

  trait Op[X]

  // all effect calls start out as idiomatic programs
  def send[X](op: Op[X]): Idiom[X] = Idiom.Impure[X, X](op, Idiom.pure(identity))
  def pure[A](value: A): Idiom[A] = Idiom.pure(value)

  // used interally. Directly send a monadic effect
  private def sendM[X](op: Op[X]): Eff[X] = Eff.Impure[X, X](op, Eff.pure)

  // lift idiomatic computation into monadic computation
  def embed[A](prog: Idiom[A]): Eff[A] = sendEmbed(prog) flatMap identity

  private def sendEmbed[A](prog: Idiom[A]): Eff[Eff[A]] = prog match {
    // This is an optimization:
    // only inject bind around actual effectful computations, otherwise we
    // get spurious traces.
    case Idiom.Pure(a) => Eff.pure(Eff.pure(a))
    case prog => Eff.Impure(Embed(prog), Eff.pure)
  }

  def log[R](prog: => Eff[R]) = {
    println("---------------------")
    val before = System.currentTimeMillis()
    val result = run { prog }
    val after = System.currentTimeMillis()
    println(result)
    println(s"in ${after - before}ms")
  }


  // Base Types
  // ==========
  // We have free monad over free applicative. Is this just fraxl?
  //   https://hackage.haskell.org/package/fraxl-0.3.0.0/docs/Control-Monad-Fraxl.html
  //
  // Fraxl also nests...
  //   Fraxl r = FreerT (Union r)
  //   FreerT f = FreeT (Ap f)
  // where
  //   FreeT is a free(er) monad construction
  //   Ap is a free(er) applicative construction
  sealed trait Eff[A] {
    def map[B](f: A => B): Eff[B]
    def flatMap[B](f: A => Eff[B]): Eff[B]
    def ap[B](f: Eff[A => B]): Eff[B] = flatMap { a => f map { _ apply a } }
    def andThen[B](f: Eff[B]): Eff[B] = flatMap { _ => f }
  }
  object Eff {
    // does it make sense to have pure Monadic values, if you can have pure idiomatic ones?
    case class Pure[A](value: A) extends Eff[A] {
      def map[B](f: A => B): Eff[B] = Pure(f(value))
      def flatMap[B](f: A => Eff[B]): Eff[B] = f(value)
    }

    case class Impure[X, A](op: Op[X], continuation: X => Eff[A]) extends Eff[A] {
      def map[B](f: A => B): Eff[B] = Impure(op, x => continuation(x) map f)
      def flatMap[B](f: A => Eff[B]): Eff[B] = Impure(op, x => continuation(x) flatMap f)
    }
    def pure[A](a: A): Eff[A] = Pure(a)
  }

  // The free applicative
  //   https://hackage.haskell.org/package/free-5.1/docs/Control-Applicative-Free.html
  sealed trait Idiom[A] {
    def ap[B](f: Idiom[A => B]): Idiom[B]
    def map[B](f: A => B): Idiom[B]
    def andThen[B](f: Idiom[B]): Idiom[B] = ap(f map { b => a: A => b })
    def map2[B, C](mb: Idiom[B])(f: (A, B) => C): Idiom[C] = ap(mb.map { b: B => a: A => f(a, b) })

    def fold[R](zero: R)(f: Op[_] ~> (R => R)): R
  }
  object Idiom {

    case class Pure[A](value: A) extends Idiom[A] {
      def map[B](f: A => B): Idiom[B] = Pure(f(value))
      def ap[B](ef: Idiom[A => B]): Idiom[B] = ef map { f => f(value) }
      def fold[R](zero: R)(f: Op[_] ~> (R => R)): R = zero
    }

    // there are obviously more efficient implementations, like
    // using list representations and difference lists and so on.
    //   https://www.eyrie.org/~zednenem/2013/05/27/freeapp
    //   https://hackage.haskell.org/package/free-5.1/docs/Control-Applicative-Free-Fast.html
    case class Impure[X, A](op: Op[X], continuation: Idiom[X => A]) extends Idiom[A] {
      def map[B](f: A => B): Idiom[B] =
        Impure(op, continuation map { f compose _ })
      def ap[B](ef: Idiom[A => B]): Idiom[B] =
        Impure(op, continuation ap (ef map { (f : A => B) => (g : X => A) => f compose g }))

      def fold[R](zero: R)(f: Op[_] ~> (R => R)): R = {
        if (f.isDefinedAt(op))
          continuation.fold(f(op)(zero))(f)
        else
          continuation.fold(zero)(f)
      }
    }

    def pure[A](a: A): Idiom[A] = Pure(a)
  }



  // Interpreters
  // ============


  // Idiomatic Handlers
  // ------------------

  trait Idiomatic[G[_]] {
    def onPure[A]: Idiom[A] => Idiom[G[A]]

    // this is a simplified form of Idiom[G[X => R]] => Idiom[G[R]]
    def map[A, B]: G[A] => (A => B) => G[B]

    // note that we are not only parametric in the effect result X, but also in R
    def onEffect[X, R]: Op[X] ~> (Idiom[G[X => R]] => Idiom[G[R]])

    def lifted[X, R](impl: Op[X] ~> (G[X => R] => G[R])): Op[X] ~> (Idiom[G[X => R]] => Idiom[G[R]]) = {
      case op if impl.isDefinedAt(op) => _.map { impl(op) }
    }

    def apply[R](prog: Idiom[R]): Idiom[G[R]] = runIdiomatic(this)(prog)
    def dynamic[R](prog: Eff[R])(sequence: Sequencer[G, R]): Eff[R] = effekt.dynamic(this, sequence)(prog)
  }


  private def runIdiomatic[R, G[_]](interpreter: Idiomatic[G])(prog: Idiom[R]): Idiom[G[R]] = prog match {
    case p @ Idiom.Pure(_) =>
      interpreter.onPure(prog)
    case Idiom.Impure(op, k) if interpreter.onEffect isDefinedAt op =>
      interpreter.onEffect(op)(interpreter(k))
    case Idiom.Impure(op, k) =>
      // here we require G to be a functor to apply `gk: G[x => R]` to `x` (that is convert to `x => G[R]`):
      Idiom.Impure(op, interpreter(k) map { gk => x => interpreter.map(gk) { xr => xr(x) } })
  }

  // Monadic Handlers
  // ----------------
  opaque type EffOp[R] = Eff[R]

  // monadic handler
  def handler[R](h: PartialFunction[EffOp[R], Eff[R]]): Eff[R] => Eff[R] = handler[R, R](x => Eff.pure(x))(h)

  // TODO duplicated code with runMonadic
  def handler[R, Res](pure: R => Eff[Res])(h: PartialFunction[EffOp[Res], Eff[Res]]): Eff[R] => Eff[Res] = {
    case Eff.Pure(v) =>
      pure(v)
    case e @ Eff.Impure(op, km) =>
      val handled = Eff.Impure(op, x => handler(pure)(h) { km(x) } )
      if (h isDefinedAt handled)
        h(handled)
      else
        handled
  }

  object -> {
    def unapply[R, X](c: EffOp[R]): Option[(Op[X], X => Eff[R])] = c match {
      case Eff.Impure(op: Op[X], km) => Some((op, km))
      case _ => None
    }
  }

  trait Monadic[R] {
    def onEffect[X]: Op[X] ~> ((X => Eff[R]) => Eff[R])
    def apply(prog: Eff[R]): Eff[R] = runMonadic(this)(prog)
  }
  private def runMonadic[R](interpreter: Monadic[R])(prog: Eff[R]): Eff[R] = prog match {
    case p @ Eff.Pure(_) =>
      p
    case Eff.Impure(op, km) if interpreter.onEffect isDefinedAt op =>
      interpreter.onEffect(op) { x => runMonadic(interpreter) { km(x) } }
    case Eff.Impure(op, km) =>
      Eff.Impure(op, x => runMonadic(interpreter) { km(x) })
  }

  // Idiom Injection
  // ===============
  // the first bind on an idiomatic computation sends a "Bind"-effect.

  // Embed has return type Eff[A] to allow sending effectful values back to the original position. (Time traveling control!)
  case class Embed[A](ap: Idiom[A]) extends Op[Eff[A]]

  // Default handler for Bind:
  // It translates applicative effects into monadic effects.
  private class EmbedDefault[R] extends Monadic[R] {
    def onEffect[X] = {
      case Embed(Idiom.Pure(a)) => resume =>
        resume(Eff.Pure(a))
      case Embed(Idiom.Impure(op, k)) => resume =>
        resume(Eff.Impure(op, x => embed(k map { _ apply x })))
    }
  }
  private def EmbedDefault[R] = new EmbedDefault[R]

  def run[A](ma: Eff[A]): A = EmbedDefault { ma } match {
    case Eff.Pure(a) => a
    case _ => sys error "Cannot run program with unhandled effects: " + ma
  }


  // Mediating between Idiomatic and Monadic Computation
  // ---------------------------------------------------

  // Defines how to sequence (embed) idiomatic computation into monadic computation.
  trait Sequencer[G[_], R] {
    // TODO think about changing to
    //   Idiom[G[X]] => (Idiom[X] => Eff[R]) => Eff[R]
    def apply[X]: G[X] => (X => Eff[R]) => Eff[R]
  }

  trait DynamicHandler[R, G[_]] {
    def handle[X]: Idiom[X] => Idiom[G[X]]
    def sequence[X]: G[X] => (X => Eff[R]) => Eff[R]

    // to optimize injection points, override this function
    def handles: Op[_] => Boolean = op => true

    def apply[X](prog: Idiom[X]) = handle[X](prog)
    def apply(prog: Eff[R]) = dynamic[R, G](this)(prog)
  }

  // This is just to demonstrate the nature of dynamic handlers.
  // Dynamic handlers are really just pairs of an idiomatic handler
  //   Idiomatic[G]
  // that handles some effect into the domain G and a monadic handler
  //   Monadic[R]
  // that handles G effects. G itself can be understood as an effect
  // operation, that is raised after locally handling with the idiomatic
  // handler.
  //
  // That is, the program
  //   embed { (prog: Idiom[X]) } flatMap k
  // is conceptually rewritten to
  //   embed { ih { prog } } flatMap { gx => sendM(gx) } flatMap k
  case class FromHandlers[G[X] <: Op[X], R](
      ih: Idiomatic[G] /* raises G */,
      im: Monadic[R]   /* handles G */
  ) extends DynamicHandler[R, G] {
    def handle[X] = ih.apply[X]
    def sequence[X] = { case op if im.onEffect.isDefinedAt(op) => resume => im.onEffect(op)(resume) }
    override def handles = ih.onEffect.isDefinedAt(_)
  }

  // The combinator `dynamic` injects the provided interpreter at the position of the
  // first call to flatMap on an idiomatic program. This implies that also all effects
  // used by the interpreter are evaluated at that particular position!
  def dynamic[R, G[_]](handler: DynamicHandler[R, G]): Monadic[R] = new Monadic[R] { outer =>

    // collects the continuation
    private case class Dynamic[R](gr: G[R]) extends Op[R] { val prompt = outer }

    // This is an optimization:
    //   Since this is purely driven by "the first bind" it might insert too many calls to an interpreter.
    //   Should the interpreter be wrapped around a term that does not use the corresponding effect?
    //   It's an applicative term: We can just search for effects that are handled by the interpreter!
    private def shouldHandle[A](prog: Idiom[A]): Boolean = prog.fold(false) {
      case op => handler.handles(op) || _
    }

    def onEffect[X] = {

      case Embed(prog) if shouldHandle(prog) => resume => {

        // 1) inject idiomatic handler
        val handled = handler.handle { prog }

        // 2) give outer handlers the chance to handle the remaining idiomatic computation
        val rebound = sendEmbed(handled)

        // Send resulting computation back to the future. It ...
        // 3) runs the idiomatic handler in the original position of the bind
        // 4) collects the continuation (using Dynamic) to be sequenced in a separate step
        //
        // Note:
        //   using sendM here is important since otherwise we need to call `embed` and end
        //   up in this very handler again.
        rebound flatMap { prog => resume(prog flatMap { ga => sendM(new Dynamic(ga)) }) }
      }

      case d : Dynamic[r] if d.prompt eq this => resume =>
        handler.sequence(d.gr)(resume)
    }
  }

  // The combinator `dynamic` injects the provided interpreter at the position of the
  // first call to flatMap on an idiomatic program. This implies that also all effects
  // used by the interpreter are evaluated at that particular position!
  def dynamic[R, G[_]](interpreter: Idiomatic[G], sequencer: Sequencer[G, R]): Monadic[R] =
    dynamic[R, G](new DynamicHandler {
      def handle[X] = interpreter.apply[X]
      def sequence[X] = sequencer.apply[X]
    })

  def dynamic[R](shouldHandle: Op[_] => Boolean)(seq: Sequencer[Idiom, R]): Monadic[R] =
    dynamic(new Applicable[Idiom] {
      def interpret[X] = { case op if shouldHandle(op) => send(op) }
    }, seq)


  // Helpers
  // =======

  trait IdiomaticId extends Idiomatic[[X] =>> X] {
    def onPure[A] = a => a
    def map[A, B] = a => f => f(a)
  }


  // Cats Interop
  // ============
  import cats.{ Applicative, Functor, Monoid, Monad }
  import cats.implicits._

  implicit object effMonad extends Monad[Eff] {
    override def flatMap[A, B](fa: Eff[A])(f: A => Eff[B]): Eff[B] = fa flatMap f
    override def pure[A](a: A): Eff[A] = Eff.pure(a)

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
    def pure[A](a: A): Idiom[A] = Idiom.pure(a)
    override def map[A, B](fa: Idiom[A])(f: A => B): Idiom[B] = fa map f
  }

  trait Functorial[F[_]: Functor] extends Idiomatic[F] {
    type G[X] = F[X]
    def map[A, B] = Functor[F].map
  }

  trait Monoidal[D](implicit val m: Monoid[D]) extends Idiomatic[[X] =>> D] {
    def onPure[R] = r => pure(m.empty)
    def map[A, B] = d => f => d
  }

  // since Applicative is already taken, we use the made up name "Applicable"
  trait Applicable[F[_]: Applicative] extends Idiomatic[F] {
    // `interpret` is a natural transformation for a subset of Op to F
    def interpret[X]: Op[X] ~> F[X]
    def onPure[R] = _ map Applicative[F].pure
    def map[A, B] = Applicative[F].map
    def onEffect[X, R] = { case op if interpret.isDefinedAt(op) => resume =>
      resume map { Applicative[F].ap(_)(interpret(op)) }
    }
  }
}

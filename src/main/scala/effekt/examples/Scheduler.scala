package effekt
package examples

import utils._

/**
 * Without builtin state
 */
object scheduler extends App {

  // can we change the interface of Yield to allow GSS and GLL implementations?
  // something like indexing suspend with a position

  // before we implement breadth first, let's first implement a scheduler.
  trait Process extends Eff {
    def suspend(): Unit / effect
    def fork(): Boolean / effect
  }
  def Process given (P: Process): P.type = P

  // Y could be called "(Yield)Pool"

  def myFork given Process = for {
    _ <- log("in fork")
    _ <- Process.suspend()
    _ <- log("in fork: after suspend")
    _ <- Process.suspend()
    _ <- log("in fork: again, after suspend")
  } yield ()

  def myMain given Process = for {
    _ <- log("in main")
    _ <- Process.suspend()
    _ <- log("in main: after suspend")
  } yield ()

  def prog given Process = for {
    _ <- log("before fork")
    forked <- Process.fork()
    _ <- if (forked) myFork else myMain
    _ <- log("shared continuation")
  } yield ()

  case class Pool[E](run: List[Pool[E]] => Unit / E)
  def execute[E](ps: List[Pool[E]]): Unit / E = ps match {
    case Nil     => pure(())
    case p :: ps => p run ps
  }

  class Scheduler[FX] extends Process with Handler[Unit, Pool[FX]] {

    type effects = FX

    def unit = _ => pure { Pool(ps => execute(ps)) }

    def fork(): Boolean / effect = use { pure { Pool { ps => for {
      forked <- resume(true)
      main   <- resume(false)
      result <- execute { forked :: main :: ps } // == forked(main :: ps)
    } yield result }}}

    def suspend(): Unit / effect = use { pure { Pool { ps => for {
      suspended <- resume(())
      result    <- execute { ps :+ suspended } // p(ps :+ suspended) where ps = p :: ps
    } yield result }}}
  }

  println {
    run {
      new Scheduler handle {
        prog
      } flatMap { p => execute(List(p)) }
    }
  }
}


/**
 * With builtin state
 * (more up to date implementation)
 */
object schedulerState extends App {

  // can we change the interface of Yield to allow GSS and GLL implementations?
  // something like indexing suspend with a position

  // before we implement breadth first, let's first implement a scheduler.
  trait Process extends Eff {
    def suspend(): Unit / effect
    def fork(): Boolean / effect
    def exit(): Nothing / effect

    // philipp's fork
    //
    // {
    //   val forked <- fork()
    //   if (forked) { do p; do exit(); pure(()) } else pure(())
    // }
    //
    def fork[E](p: Unit / E): Unit / (E & effect) = for {
      forked <- fork()
      res    <- if (forked) p andThen exit() else pure(())
    } yield res

    // TODO dangerous! this usage of real state doesn't integrate well with other ambiguity effects!!!
    // Use ambient state instead!
    def interleave[A, B, E](p: A / E, q: B / E): (A, B) / (E & effect) = {
      var a: Option[A] = None
      var b: Option[B] = None

      def pa: Unit / (E & effect) = p map { ra => a = Some(ra) }
      def pb: Unit / (E & effect) = q map { rb => b = Some(rb) }

      // this is busy waiting
      def waiting: (A, B) / (E & effect) =
        if (a.isDefined && b.isDefined) {
          pure((a.get, b.get))
        } else {
          // do suspend(); waiting
          suspend() flatMap { _ => waiting }
        }

      for {
        _ <- fork(pa)
        _ <- fork(pb)
        r <- waiting
      } yield r
    }
  }
  def Process given (P: Process): P.type = P

  def myFork given Process = for {
    _ <- log("in fork")
    _ <- Process.suspend()
    _ <- log("in fork: after suspend")
    _ <- Process.suspend()
    _ <- log("in fork: again, after suspend")
  } yield ()

  def myMain given Process = for {
    _ <- log("in main")
    _ <- Process.suspend()
    _ <- log("in main: after suspend")
  } yield ()

  def prog given Process = for {
    _ <- log("before fork")
    forked <- Process.fork()
    _ <- if (forked) myFork else myMain
    _ <- log("shared continuation")
  } yield ()

  class Scheduler[FX] extends Process with StatefulHandler[Unit, Unit] {

    type effects = FX

    type Pool = Unit / Effects

    val ps = Field[List[Pool]](Nil)

    def unit = _ => pure(())

    def run(): Unit / Effects = ps.value flatMap {
      case Nil => pure(())
      case process :: rest => for {
        _ <- ps.value = rest
        _ <- process
        _ <- run()
      } yield ()
    }

    def exit() = use { pure(()) }

    def fork() = use { for {
      _ <- ps update { resume(true) :: resume(false) :: _ }
      _ <- run()
    } yield () }

    def suspend() = use { for {
      _ <- ps update { _ :+ resume(()) }
      _ <- run()
    } yield () }
  }

  def interleaveTest given Process = for {
    (a, b) <- Process.interleave(Process.interleave(for {
        () <- Process.suspend()
        () <- log("in first (1)")
        () <- Process.suspend()
        () <- log("in first (2)")
      } yield 1, for {
        () <- log("in second (1)")
        () <- Process.suspend()
        () <- log("in second (2)")
        () <- Process.suspend()
        () <- log("in second (3)")
        () <- Process.suspend()
        () <- log("in second (4)")
        () <- Process.suspend()
        () <- log("in second (5)")
        () <- Process.suspend()
        () <- log("in second (6)")
        () <- Process.suspend()
      } yield true), for {
      () <- log("in third (1)")
      () <- Process.suspend()
      () <- log("in third (2)")
      () <- Process.suspend()
      () <- log("in third (3)")
      () <- Process.suspend()
      () <- log("in third (4)")
      () <- Process.suspend()
    } yield true)
  } yield if (b) a else 42


  run {
    new Scheduler handle { for {
      res <- interleaveTest
      ()  <- log(res.toString)
    } yield () }
  }

}
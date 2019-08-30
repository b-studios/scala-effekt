package examples.unsafe

import effekt.unsafe._
import utils._

object scheduler extends App {

  // EFFECT SIGNATURES
  // =================
  trait Fiber {
    def suspend(): Control[Unit]
    def fork(): Control[Boolean]
    def exit(): Control[Nothing]

    // derived effects
    def fork(p: Control[Unit]): Control[Unit] = for {
      forked <- fork()
      res    <- if (forked) p andThen exit() else pure(())
    } yield res
  }
  def Fiber given (F: Fiber): F.type = F

  trait Async {
    type Promise[T]
    def async[T](prog: Control[T]): Control[Promise[T]]
    def await[T](p: Promise[T]): Control[T]
  }
  def Async given (A: Async): A.type = A


  // EFFECT HANDLERS
  // ===============
  trait Scheduler given State given Scope[Unit] extends Fiber {

    type Queue = List[Control[Unit]]

    private lazy val ps = Field[Queue](Nil)

    def exit() = scope { pure(()) }

    def fork() = scope {
      ps.update { resume(true) :: resume(false) :: _ } andThen run
    }

    def suspend() = scope {
      ps.update { _ :+ resume(()) } andThen run
    }

    def run: Control[Unit] = ps.value flatMap {
      case Nil => pure(())
      case process :: rest => (ps.value = rest) andThen process andThen run
    }
  }

  def scheduler(prog: given Fiber => Control[Unit]) given State =
    handle[Unit] { prog given new Scheduler {} }

  def schedule(prog: given Fiber => Control[Unit]) = region { scheduler { prog } }

  class Poll given (val s: State) given Fiber extends Async {
    // XXX opaque does not work here?
    type Promise[T] = s.Field[Option[T]]

    def async[T](prog: Control[T]) = {
      val p = Field[Option[T]](None)
      Fiber.fork { prog map { Some(_) } flatMap p.value_= } map { _ => p }
    }
    def await[T](p: Promise[T]) = p.value flatMap {
      case Some(r) => pure(r)
      case None => Fiber.suspend() andThen await(p)
    }
  }
  def poll[R, E](prog: given Async => Control[R]) given Fiber given State =
    prog given new Poll

  // EXAMPLES
  // ========

  def ex given Fiber given Async = for {
    p <- Async.async { for {
        _ <- log("Async 1")
        _ <- Fiber.suspend()
        _ <- log("Async 2")
        _ <- Fiber.suspend()
        _ <- log("Async 3")
        _ <- Fiber.suspend()
        _ <- log("Async 4")
      } yield 42 }
    _ <- log("Main 1")
    _ <- Fiber.suspend()
    _ <- log("Main 2")
    r <- Async.await(p)
    _ <- log("Main 3 with result " + r)
  } yield ()

  run {
    region {
      scheduler {
        poll {
          ex
        }
      }
    }
  }

  def join[S, T](ps: Control[S], pt: Control[T]) given Async = for {
    pps <- Async.async { ps }
    ppt <- Async.async { pt }
    s <- Async.await(pps)
    t <- Async.await(ppt)
  } yield (s, t)

  def interleaveTest given Fiber given Async = for {
      ((a, x), b) <- join(join(for {
          () <- Fiber.suspend()
          () <- log("in first (1)")
          () <- Fiber.suspend()
          () <- log("in first (2)")
        } yield 1, for {
          () <- log("in second (1)")
          () <- Fiber.suspend()
          () <- log("in second (2)")
          () <- Fiber.suspend()
          () <- log("in second (3)")
          () <- Fiber.suspend()
          () <- log("in second (4)")
          () <- Fiber.suspend()
          () <- log("in second (5)")
          () <- Fiber.suspend()
          () <- log("in second (6)")
          () <- Fiber.suspend()
        } yield true), for {
        () <- log("in third (1)")
        () <- Fiber.suspend()
        () <- log("in third (2)")
        () <- Fiber.suspend()
        () <- log("in third (3)")
        () <- Fiber.suspend()
        () <- log("in third (4)")
        () <- Fiber.suspend()
      } yield true)
    } yield if (b) a else 42

  run {
    region {
      scheduler {
        poll {
          interleaveTest flatMap log
        }
      }
    }

  }

  def myFork given Fiber = for {
    _ <- log("in fork")
    _ <- Fiber.suspend()
    _ <- log("in fork: after suspend")
    _ <- Fiber.suspend()
    _ <- log("in fork: again, after suspend")
  } yield ()

  def myMain given Fiber = for {
    _ <- log("in main")
    _ <- Fiber.suspend()
    _ <- log("in main: after suspend")
  } yield ()

  def prog given Fiber = for {
    _ <- log("before fork")
    forked <- Fiber.fork()
    _ <- if (forked) myFork else myMain
    _ <- log("shared continuation")
  } yield ()
}
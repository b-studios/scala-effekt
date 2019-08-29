package examples

import effekt._
import utils._

object scheduler extends App {

  // EFFECT SIGNATURES
  // =================
  trait Fiber extends Eff {
    def suspend(): Control[Unit, effect]
    def fork(): Control[Boolean, effect]
    def exit(): Control[Nothing, effect]

    // derived effects
    def fork[E](p: Control[Unit, E]): Control[Unit, E & effect] = for {
      forked <- fork()
      res    <- if (forked) p andThen exit() else pure(())
    } yield res
  }
  def Fiber given (F: Fiber): F.type = F

  trait Async extends Eff {
    type Promise[T]
    def async[T, E](prog: Control[T, E]): Control[Promise[T], E & effect]
    def await[T](p: Promise[T]): Control[T, effect]
  }
  def Async given (A: Async): A.type = A


  // EFFECT HANDLERS
  // ===============
  trait Scheduler[E] extends Fiber {

    val state: State
    val scope: Scope[Unit, state.effect & E]
    type effect = scope.effect

    type Queue = List[Unit / (state.effect & E)]

    lazy val ps = state.Field[Queue](List.empty)

    def exit() = scope { resume => pure(()) }

    def fork() = scope { resume =>
      ps.update { resume(true) :: resume(false) :: _ } andThen run
    }

    def suspend() = scope { resume =>
      ps.update { _ :+ resume(()) } andThen run
    }

    def run: Control[Unit, state.effect & E] = ps.value flatMap {
      case Nil => pure(())
      case process :: rest => (ps.value = rest) andThen process andThen run
    }
  }
  def scheduler[E](st: State)(prog: (f: Fiber) => Control[Unit, f.effect & E]): Control[Unit, st.effect & E] =
    handle[Unit, st.effect & E] { sc =>
      prog(new Scheduler[E] { val scope: sc.type = sc; val state: st.type = st })
    }

  def schedulerI[E](prog: given (f: Fiber) => Unit / (f.effect & E)) given State =
    handle[Unit, State.effect & E] { s =>
      prog given new Scheduler[E] { val scope: s.type = s; val state: State.type = State }
    }

  trait Poll extends Async {
    val state: State
    val fiber: Fiber

    type effect = state.effect & fiber.effect

    type Promise[T] = state.Field[Option[T]]

    def async[T, E](prog: Control[T, E]): Control[Promise[T], E & effect] = {
      val p = state.Field[Option[T]](None)
      fiber.fork { prog map { Some(_) } flatMap p.value_= } map { _ => p }
    }
    def await[T](p: Promise[T]) = p.value flatMap {
      case Some(r) => pure(r)
      case None => fiber.suspend() andThen await(p)
    }
  }

  def poll[R, E](s: State, f: Fiber)(prog: (a: Async) => R / (a.effect & E)): R / (E & s.effect & f.effect) =
    prog(new Poll { val state: s.type = s; val fiber: f.type = f })

  // using implicits
  def pollI[R, E](prog: given (a: Async) => R / (a.effect & E)) given Fiber given State =
    prog given new Poll { val state: State.type = State; val fiber: Fiber.type = Fiber }

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

  def asyncEx(f: Fiber, a: Async) = for {
    p <- a.async { for {
        _ <- log("Async 1")
        _ <- f.suspend()
        _ <- log("Async 2")
        _ <- f.suspend()
      } yield 42 }
    _ <- log("Main")
    r <- a.await(p)
    _ <- log("Main with result " + r)
  } yield ()

  run {
    region { s =>
      scheduler[s.effect](s) { f =>
        poll(s, f) { a =>
          asyncEx(f, a)
        }
      }
    }
  }

  def join[S, T, E](ps: S / E, pt: T / E) given Async = for {
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
    region { s =>
      scheduler[s.effect](s) { f =>
        poll(s, f) { a =>
          (interleaveTest given f given a) flatMap log
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
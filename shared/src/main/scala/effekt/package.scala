package object effekt {

  // Markers
  // ===
  // previously called "Prompt"
  sealed trait Marker

  /**
   * Marker used on the stack to delimit captured continuations
   */
  @scala.annotation.implicitNotFound("No continuation marker found for 'use'. Maybe you forgot to handle the effect?")
  trait ContMarker[Res] extends Marker

  /**
   * Marker used on the stack to store ambient state (delimited dynamic state)
   */
  trait StateMarker extends Marker {
    type StateRep
    def backup: StateRep
    def restore(value: StateRep): Unit
  }

  /**
   * Marker used on the stack to store exception handlers for interaction with native exceptions
   */
  trait CatchMarker[Res] extends Marker {
    def _catch: PartialFunction[Throwable, Control[Res]]
  }


  // Aliases
  // ===

  private[effekt] type Frame[-A, +B] = A => Control[B]

  type using[+A, -E] = E => Control[A]

  type CPS[+A, E] = (A => Control[E]) => Control[E]


  // Main API
  // ===

  final def pure[A](a: => A): Control[A] = new Trivial(a)

  final def run[A](c: Control[A]): A = c.run()

  final def handle[Res](h: Handler[Res])(f: Res using h.type): Control[Res] = h.handle(f)


  // Low Level API
  // ===

  // Continuations
  final def handling[Res](f: Res using ContMarker[Res]): Control[Res] = Control.delimitCont(new ContMarker[Res] {})(f)

  final def use[A, Res](body: CPS[A, Res])(implicit p: ContMarker[Res]) = Control.use(p) { body }

  // State
  final def region[R](prog: State => Control[R]): Control[R] = {
    val s = new State {}
    Control.delimitState(s) { prog(s) }
  }

  // Catch
  final def _try[Res](prog: Control[Res])(handler: PartialFunction[Throwable, Control[Res]]): Control[Res] =
    prog _catch handler
}

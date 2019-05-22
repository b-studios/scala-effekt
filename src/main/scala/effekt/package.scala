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

  type using[+A, -E] = given E => Control[A]

  type and[+A, -E] = given E => A

  type CPS[A, E] = given (A => Control[E]) => Control[E]

  final def resume[A, Res](a: A): CPS[A, Res] = given k => k(a)

  final def resume[A, Res, S](a: A, s: S) given (k: ((A, S) => Control[Res])): Control[Res] = k(a, s)


  final def pure[A](a: => A): Control[A] = new Trivial(a)

  final def run[A](c: Control[A]): A = c.run()

  final def handle[Res](h: Handler[Res])(f: Res using h.type): Control[Res] = h.handle(f)


  // Low Level API
  // ===

  // Continuations
  // ===
  final def handling[Res](f: Res using ContMarker[Res]): Control[Res] = Control.handle(new ContMarker[Res] {})(f)

  def use[A, Res](body: CPS[A, Res]) given (p: ContMarker[Res]) = Control.use(p) { body }

  // State
  // ===
  def stateful[S, R](init: S)(body: Stateful[S] => Control[R]): Control[R] = {
    val state = new Stateful[S] {
      private var state: S = init
      def get(): S = state
      def put(s: S): Unit = state = s
    }

    Control.stateful(state) { body }
  }
}

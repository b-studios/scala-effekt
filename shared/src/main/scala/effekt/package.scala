package object effekt {

  private[effekt] type Frame[-A, +B] = A => Control[B]


  // previously called "Prompt"
  @scala.annotation.implicitNotFound("No continuation marker found for 'use'. Maybe you forgot to handle the effect?")
  trait ContMarker[Res]

  trait StateMarker {
    type StateRep
    def backup: StateRep
    def restore(value: StateRep): Unit
  }

  trait CatchMarker[Res] {
    def _catch: PartialFunction[Throwable, Control[Res]]
  }

  type using[+A, -E] = E => Control[A]

  type CPS[+A, E] = (A => Control[E]) => Control[E]

  final def pure[A](a: => A): Control[A] = new Trivial(a)

  final def run[A](c: Control[A]): A = c.run()

  final def handle[R, Res](h: Handler[R, Res])(f: R using h.type): Control[Res] = h.handle(f)


  // Lowlevel API

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

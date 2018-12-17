package object effekt {

  trait Eff { type effect }

  // we use intersection types to track effects, so Pure = Top
  type Pure = Any

  // A type DSL to construct implicit, dependent function types
  // Currently effectively blocked by https://github.com/lampepfl/dotty/issues/5288
  type /[+A, -E] = Control[A, E]

  type CPS[A, R] = implicit (A => R) => R

  final def run[A](c: A / Pure): A = Result.trampoline(c(ReturnCont()))


  @forceInline
  final def pure[A](a: => A): A / Pure = new Trivial(a)

  final def resume[A, R](a: A): CPS[A, R] = implicit k => k(a)

  // Just a marker trait used by the delimcc implementation
  trait Prompt {
    type Result
    type Effects
    type effect = this.type
  }

  // delimited continuations
  // ===
  @scala.annotation.implicitNotFound("No enclosing handler found. Maybe you forgot to handle the effect?")
  trait Effect extends Prompt { effect =>
    def apply[A](body: CPS[A, Result / Effects]): A / effect.type = Control.use(this) { implicit k => body(k) }

    // for backwards compatability
    def use[A](body: CPS[A, Result / Effects]): A / effect.type = apply { body }
  }

  final def handler[R, E](f: (e: Effect { type Result = R; type Effects = E }) => R / (e.type & E)): R / E = {
    val e = new Effect { type Result = R; type Effects = E }
    Control.delimitCont(e) { f(e) }
  }


  // delimited dynamic state
  // ===
  // State is just a built-in version, optimized for fast getting and setting.
  // Additional cost per continuation capture (for backup/restore).
  // With the same public interface we could trade fast capture for slow lookup/write.
  //
  // The state interface is important since it separates the delimited from the type of
  // state. This is not the case with the traditional `State[S]` interface. The separation
  // is necessary to allow typing the scheduler example.
  //
  // The state effect is parametric in Result and Effects!
  sealed trait State { state =>

    def init[T](value: T): Field[T] = {
      val field = new Field[T]()
      data = data.updated(field, value)
      field
    }

    private[effekt] type StateRep = Map[Field[_], Any]
    private[effekt] var data = Map.empty[Field[_], Any]
    private[effekt] def backup: StateRep = data
    private[effekt] def restore(value: StateRep): Unit = data = value

    // all the field data is stored in `data`
    class Field[T] private[State] () {
      def value: T / state.type = pure(data(this).asInstanceOf[T])
      def value_=(value: T): Unit / state.type = pure {
        data = data.updated(this, value)
      }
      def update(f: T => T): Unit / state.type = for {
        old <- value
        _   <- value_=(f(old))
      } yield ()
    }
  }

  def region[R, E](prog: (s: State) => R / (s.type & E)): R / E = {
    val s = new State {}
    Control.delimitState(s) { prog(s) }
  }

  // The preferred API: prompt composition
  // While Handler[R, E] is nice for parsers that don't have dependencies
  // to other effects, type members are better suited for this use case
  // (see Stateful, or parsers.BreadthFirst2)

  // since path dependency on trait-parameters is currently buggy,
  // we can't define the dependency on other effects via parameters. So, instead
  // of
  //    trait MyHandler[R, E](val exc: Exc) extends Handler[R, E & exc.effect] { ... }
  // we have to write
  //    trait MyHandler[R, E] extends Effectful {
  //      type Result  = R
  //      type Effects = E & exc.effect
  //    }
  trait Effectful extends Eff { outer =>
    type Result
    type Effects

    val effect: Effect { type Result = outer.Result; type Effects = outer.Effects }
    type effect = effect.type
  }

  trait Handler[R, E] extends Effectful {
    type Result = R; type Effects = E
  }

  trait Stateful[R, E] extends Eff {
    val state: State
    type state  = state.type

    val effect: Effect { type Result = R; type Effects = state & E }
    type effect = effect.type
  }


  // Here, handlers ARE prompt markers
  // This is unsafe, since it does not guarantee encapsulation. It does guarantee
  // effect safety, though (all effects are handled).
  object unsafe {
    trait Effectful extends Effect {
      val effect: this.type = this
      type effect = effect.type
    }

    trait Handler[R, E] extends Effectful {
      type Result = R
      type Effects = E
    }

    trait Stateful[R, E] extends Effectful with State {

      type Result = R
      type Effects = state.type & E

      // we purposefully lose some type information by upcasting here to
      // prevent including this.type in Effects.
      // Otherwise, we might accidentally use `effect { effect { ... } }` which
      // shouldn't be possible since we have -F+.
      val state: State = this
      type state = state.type
    }

    // This version is very convenient for handler-authors, since the handler instance can be the prompt
    // itself. However, it might violate encapsulation by using the same handler instance multiple
    // times.
    def handle(e: Effectful)(prog: e.type => e.Result / (e.type & e.Effects)): e.Result / e.Effects =
      Control.delimitCont(e) { prog(e) }

    def handleStateful[R, E](e: Stateful[R, E])(prog: e.type => R / (e.type & E)): R / E =
      Control.delimitState[R, E](e) {
        Control.delimitCont(e) {
          prog(e)
        }.asInstanceOf[R / (e.type & E)]
      }
  }

  // internally we ignore the effects
  private[effekt] type Frame[-A, +R] = A => Control[R, _]
}

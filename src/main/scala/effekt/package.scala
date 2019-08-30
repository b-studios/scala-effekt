package object effekt {

  // Marker trait to say: "this type is effectful"
  trait Eff { type effect }

  // we use intersection types to track effects, so Pure = Top
  type Pure = Any

  // A type DSL to construct implicit, dependent function types
  // Currently effectively blocked by https://github.com/lampepfl/dotty/issues/5288

  // TODO deprecate / in favor of Control
  type /[+A, -E] = Control[A, E]

  type The[A] = A & Singleton

  // The usual suspects
  // ===
  final def pure[A](a: => A): Control[A, Pure] = new Trivial(a)

  type CPS[+A, R] = given (A => R) => R
  inline def resume[A, R, E](a: A) given (k: A => Control[R, E]): Control[R, E] = k(a)

  final def run[A](c: Control[A, Pure]): A = Result.trampoline(c(ReturnCont()))

  // delimited control
  // ===

  // Just a marker trait used by the delimcc implementation
  trait Scope[R, E] extends Eff {
    type effect = this.type // just to type check the core
    def apply[A](body: CPS[A, Control[R, E]]): Control[A, effect] = switch(body)
    def switch[A](body: CPS[A, Control[R, E]]): Control[A, effect]
  }
  inline def Scope[R, E] given (s: Scope[R, E]): s.type = s

  def handle[R, E](prog: given (c: Scope[R, E]) => R / (c.effect & E)): R / E = {
    val scope = new Scope[R, E] {
      type effect = this.type
      def switch[A](body: CPS[A, Control[R, E]]): Control[A, effect] = Control.shift(this)(body)
    }
    Control.resetWith(scope) { prog given scope }
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
  trait State extends Eff {

    def Field[T](value: T): Field[T] = {
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
      def value: T / effect = pure(data(this).asInstanceOf[T])
      def value_=(value: T): Unit / effect = pure {
        data = data.updated(this, value)
      }
      def update(f: T => T): Unit / effect = for {
        old <- value
        _   <- value_=(f(old))
      } yield ()
    }
  }
  inline def State given (s: State): s.type = s

  def region[R, FX](prog: given (s: State) => Control[R, s.effect & FX]): Control[R, FX] = {
    val s = new State {}
    Control.delimitState(s) { prog given s }
  }

  // internally we ignore the effects
  private[effekt] type Frame[-A, +R] = A => Control[R, _]
}

package object effekt {

  // we use intersection types to track effects, so Pure = Top
  type Pure = Any

  // A type DSL to construct implicit, dependent function types
  // Currently effectively blocked by https://github.com/lampepfl/dotty/issues/5288
  type /[+A, -E] = Control[A, E]

  // we drop implicit from the function types to improve type inference for now.
  type using[+A, -H]  = [-Effects] => (h: H) => A / (h.type & Effects)
  type and[C[-_], -H] = [-Effects] => (h: H) => C[h.type & Effects]
  type also[C[-_], -E] = [-Effects] => C[E & Effects]
  type Prog[C[-_]] = C[Any]

  type CPS[A, R] = implicit (A => R) => R

  final def run[A](c: A / Pure): A = c.run

  // that is Prog[h.Res using h.type also h.Effects] but the syntactic sugar impedes
  // type inference with implicit function types
  final def handle(p: Prompt)(f: p.type => p.Result / (p.type & p.Effects)): p.Result / p.Effects =
    Control.handle(p)(h => f(h))

  final def handling[R, E](f: (p: Handler[R, E]) => R / (p.type & E)): R / E = {
    val p = new Handler[R, E] { }
    Control.handle(p)(h => f(h))
  }

  final def pure[A](a: => A): A / Pure = new Trivial(a)

  final def resume[A, R](a: A): CPS[A, R] = implicit k => k(a)


  // delimited dynamic state
  // ===
  // optimize for fast getting and setting.
  // Additional cost per continuation capture (for backup/restore).
  // With the same public interface we could trade fast capture for slow lookup/write
  trait Key {
    type Value

    private var _value: Value = _

    def value: Value / this.type = pure(_value)
    def value_=(v: Value): Unit / this.type = pure { _value = v }

    private[effekt] def get: Value = _value
    private[effekt] def set(v: Value) = _value = v
  }
  // not using this additional type crashes dotty, currently
  trait State[V] extends Key { type Value = V }

  def state[V, R, E](init: V)(prog: (key: State[V]) => R / (key.type & E)): R / E = {
    val key = new State[V] {}
    key.set(init)
    Control.bind[R, E](key) { prog(key) }
  }


  // capture continuations
  // ===
  @scala.annotation.implicitNotFound("No prompt found for 'use'. Maybe you forgot to handle the effect?")
  trait Prompt {
    type Result
    type Effects

    private[effekt] type State
    private[effekt] def backup: State
    private[effekt] def restore(value: State): Unit
  }

  trait H extends Prompt {

    // we need to be careful not to confuse state.type with this.type
    // hence the new reference
    type effect = this.type
    type state = state.type
    object state

    private[effekt] type State = Map[Field[_], Any]
    private[effekt] var data = Map.empty[Field[_], Any]
    private[effekt] def backup: State = data
    private[effekt] def restore(value: State): Unit = data = value

    // all the field data is stored in `data`
    class Field[T] private () {
      def value: T / state = pure(data(this).asInstanceOf[T])
      def value_=(value: T): Unit / state = pure {
        data = data.updated(this, value)
      }
      def update(f: T => T): Unit / state = for {
        old <- value
        _   <- value_=(f(old))
      } yield ()
    }
    object Field {
      def apply[T](init: T): Field[T] = {
        val field = new Field[T]()
        data = data.updated(field, init)
        field
      }
    }

    // that's all the effects the handler might use:
    // - public effects `Effects`
    // - own state effects `state`
    //
    // it would be nice if we could override this in handlers that don't use
    // state to simplify the types.
    type HandlerEffects = state & Effects
    def use[A](body: CPS[A, Result / HandlerEffects]): A / this.type =
      Control.use(this) { implicit k =>
        body(k.asInstanceOf[A => Result / HandlerEffects]).asInstanceOf[Result / Effects]
      }
  }

  trait Handler[R, E] extends H {
    type Result = R
    type Effects = E
  }

  trait Eff { type effect }

  def use[A](implicit p: Prompt) = ContinuationScope[A, p.type](p)

  // this complication is only necessary since we can't write `use {}` and have p inferred
  // as `use(p) {}`. So we write `use in {}` to mean `use(p) in {}`
  // In summary, we use value inference to guide type inference.
  case class ContinuationScope[A, P <: Prompt](p: P) {
    def in(body: CPS[A, p.Result / p.Effects]): A / p.type = Control.use(p) { body }
    def apply(body: CPS[A, p.Result / p.Effects]): A / p.type = in(body)
  }

  // internally we ignore the effects
  private[effekt] type Frame[-A, +R] = A => Control[R, _]
}

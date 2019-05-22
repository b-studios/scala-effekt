package effekt

/**
 * Baseclass for effect handlers
 *
 * Handlers can be stateful, that is, they can carry state across
 * several handled operations. To do so, the helper trait
 * [[Handler.Stateful]] can be used.
 *
 * @see An example of an effect signature and the corresponding
 *      handler implementation can be found in the
 *      [[http://b-studios.de/scala-effekt/guides/getting-started.html getting started guide]].
 */
trait Handler[Res] extends ContMarker[Res] { h =>

  def use[A](body: CPS[A, Res]): Control[A] = Control.use(this)(body)

  def handle(prog: Res using this.type): Control[Res] = this match {
    // If the handler is stateful, also install a state frame.
    case self : State =>
      Control.delimitState(self) {
        Control.delimitCont(this) {
          prog
        }
      }
    case _ =>
      Control.delimitCont(this) { prog }
  }

  def apply(prog: Res using this.type): Control[Res] = handle(prog)
}


object Handler {

  trait Basic[Res] extends Handler[Res]

  trait Stateful[Res, S] extends Handler[Res] with State {
    val init: S
    val state = Field(init)

    def useState[A](body : S => (A => S => Control[Res]) => Control[Res]): Control[A] = use { given resume =>
      for {
        before <- state.value
        res <- body(before)(a => after => (state.value = after) andThen resume(a))
      } yield res
    }
  }
}

// for an effect safe version, see
//     https://github.com/b-studios/scala-effekt/blob/dotty-typed/src/main/scala/effekt/package.scala#L50-L85
//
// All field access and updates are "pure" in the sense of control-effects.
// They were originally wrapped in Control to track the state in the effect row.
// They are still wrapped in Control to make the API more uniform
trait State extends StateMarker {

  type StateRep = Map[Field[_], Any]
  def backup: StateRep = data
  def restore(value: StateRep): Unit = data = value

  def Field[T](value: T): Field[T] = {
    val field = new Field[T]()
    data = data.updated(field, value)
    field
  }

  private var data = Map.empty[Field[_], Any]

  // all the field data is stored in `data`
  class Field[T] private[State] () {
    def value: Control[T] = pure { data(this).asInstanceOf[T] }
    def value_=(value: T): Control[Unit] = pure {
      data = data.updated(this, value)
    }
    def update(f: T => T): Control[Unit] = for {
      old <- value
      _   <- value_=(f(old))
    } yield ()
  }
}
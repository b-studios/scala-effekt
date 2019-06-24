package effekt

// State is just a built-in version, optimized for fast getting and setting.
// Additional cost per continuation capture (for backup/restore).
// With the same public interface we could trade fast capture for slow lookup/write.
//
// The state interface is important since it separates the delimited from the type of
// state. This is not the case with the traditional `State[S]` interface. The separation
// is necessary to allow typing the scheduler example.
//
// The state effect is parametric in Result and Effects!
trait State extends Stateful {

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
    def value: T / state = pure(data(this).asInstanceOf[T])
    def value_=(value: T): Unit / state = pure {
      data = data.updated(this, value)
    }
    def update(f: T => T): Unit / state = for {
      old <- value
      _   <- value_=(f(old))
    } yield ()
  }
}
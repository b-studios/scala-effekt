package effekt

/**
 * Baseclass for effect handlers
  *
 * Handlers can be stateful, that is, they can carry state across
 * several handled operations. To do so, handlers need to
 * instantiate the abstract type member `State` to the desired type
 * of state or `Unit` if unused. Alternatively, the helper traits
 * [[Handler.Basic]] or [[Handler.Stateful]] can be used.
 *
 * @see An example of an effect signature and the corresponding
 *      handler implementation can be found in the
 *      [[http://b-studios.de/scala-effekt/guides/getting-started.html getting started guide]].
 */
trait Handler extends Eff { outer =>

  type R
  type Res
  type State

  /**
   * to implement an effect operation the handler receives:
   * a) its current state
   * b) a continuation taking the result of type A and the updated state
   */
  type Op[A] = State => (A => State => Control[Res]) => Control[Res]

  def unit: R => Res

  def unitState: State => R => Control[Res] = _ => r => pure(unit(r))

  def _catch: PartialFunction[Throwable, Control[Res]] = PartialFunction.empty

  /**
   * Handlers may perform cleanup actions (similar to finally-clauses).
   */
  def _finally: () => Unit = Handler.noCleanup

  def apply(init: State)(f: Capability { val handler: outer.type } => Control[R]): Control[Res] =
    effekt.handle(this)(init)(f)
}
object Handler {

  trait Basic[R0, Res0] extends Handler { outer =>
    type R = R0
    type Res = Res0
    type State = Unit

    def apply(f: Capability { val handler: outer.type } => Control[R]): Control[Res] =
      effekt.handle(this)(f)
  }

  trait Stateful[R0, Res0, State0] extends Handler {
    type R = R0
    type Res = Res0
    type State = State0
  }

  val noCleanup = () => ()
}

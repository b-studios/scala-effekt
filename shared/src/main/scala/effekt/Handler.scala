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
trait Handler[R, Res] extends Prompt[Res] { outer =>

  def unit: R => Control[Res]

  def use[A](body: CPS[A, Res]): Control[A] = Control.use(this)(body)

  def _catch: PartialFunction[Throwable, Control[Res]] = PartialFunction.empty

  /**
   * Handlers may perform cleanup actions (similar to finally-clauses).
   */
  def _finally: () => Unit = Handler.noCleanup

  def handle(f: R using this.type): Control[Res] =
    Control.handle(this) { h => f(this) flatMap unit }

  def apply(f: R using this.type): Control[Res] = handle(f)
}
object Handler {

  trait Basic[R0, Res0] extends Handler[R0, Res0]

  val noCleanup = () => ()
}

trait Stateful[S] {
  def get(): S
  def put(s: S): Unit

  def value: S = get()
  def value_=(s: S): Unit = put(s)
}
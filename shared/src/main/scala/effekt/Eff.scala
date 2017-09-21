package effekt

/**
 * Base class for all effect signatures.
 *
 * Effectful operations are grouped within a effect signature
 * which inherits from `Eff`. Typically when defining the
 * effect signature the abstract types [[Res]] and [[State]]
 * are left abstract.
 *
 * Effect operations with arguments of type `A`, `B` and result
 * type `C` have the form:
 *
 * {{{
 *    def op[R](x1: A, x2: B): C @@ R
 * }}}
 *
 * To handle an effect, the effect operations of the signature
 * need to be implemented. That is, a handler is an instance
 * of the effect signature.
 *
 * Handlers can instantiate the type member [[Res]] to the
 * the type the handler interprets the effect operation into.
 *
 * Handlers can be stateful, that is, they can carry state across
 * several handled operations. To do so, handlers need to
 * instantiate the abstract type member `State` to the desired type
 * of state or `Unit` if unused.
 *
 * @see An example of an effect signature and the corresponding
 *      handler implementation can be found in the
 *      [[http://b-studios.de/scala-effekt/guides/getting-started.html getting started guide]].
 */
trait Eff extends Serializable {
  type Op[A]
}
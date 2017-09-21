package effekt

/**
 * Base class for all effect signatures.
 *
 * Effectful operations are grouped within a effect signature
 * which inherits from `Eff`.
 *
 * Effect operations are marked with the abstract type constructor
 * [[Op]] which is defined in [[Handler]].
 *
 * Thus an operation with arguments of type `A`, `B` and result
 * type `C` has the form:
 *
 * {{{
 *    def op(x1: A, x2: B): Op[C]
 * }}}
 *
 * To handle an effect, the effect operations of the signature
 * need to be implemented. Typically this is achieved by defining
 * a trait/class that inherits from both, the effect signature
 * and [[Handler.Basic]] or [[Handler.Stateful]].
 *
 * @see An example of an effect signature and the corresponding
 *      handler implementation can be found in the
 *      [[http://b-studios.de/scala-effekt/guides/getting-started.html getting started guide]].
 */
trait Eff extends Serializable {
  type Op[A]
}
package effekt

import scala.util.escape._

trait Control[+A] { outer =>

  def apply[R](k: MetaCont[A, R]): R

  // Attention: It is unsafe to run control if not all effects have been handled!
  def run(): A = apply(ReturnCont(identity))

  def map[B](@local f: A => B): Control[B] = new Control[B] {
    def apply[R](k: MetaCont[B, R]): R = outer(k map f)
  }

  def flatMap[B](@local f: A => Control[B]): Control[B] = new Control[B] {
    def apply[R](k: MetaCont[B, R]): R = outer(k flatMap f)
  }
}

private[effekt]
final class Trivial[+A](a: A) extends Control[A] {
  def apply[R](k: MetaCont[A, R]): R = k(a)

  override def map[B](f: A => B): Control[B] = new Trivial(f(a))

  override def flatMap[B](f: A => Control[B]): Control[B] = f(a)

  override def run(): A = a
}

private[effekt]
trait Handler extends Serializable { outer =>

  val prompt: Capability
  val state: prompt.effect.State
  type Res = prompt.Res

  def updateWith(s: prompt.effect.State): Handler {val prompt: outer.prompt.type} =
    new Handler {
      val prompt: outer.prompt.type = outer.prompt
      val state = s
    }
}

object Control {

  private[effekt] def pure[A](a: A): Control[A] = new Trivial(a)

  private[effekt] final def handle[R](e: Eff)(init: e.State)(
    f: Capability { val effect: e.type } -> Control[R]
  ): Control[e.Out[R]] = {

    // produce a new prompt
    val p = Capability[R](e)

    // construct handler from prompt and initial state
    val h = new Handler {
      val prompt: p.type = p
      val state = init
    }

    new Control[e.Out[R]] {
      def apply[R2](k: MetaCont[e.Out[R], R2]): R2 = {
        // extract new state
        val c = f(p).flatMap { a =>
          new Control[e.Out[R]] {
            def apply[R3](k: MetaCont[e.Out[R], R3]): R3 = {
              (k: @unchecked) match {
                // Invariant: The last continuation on the metacont stack is a HandlerCont for p
                case HandlerCont(h2: H[p.type] @unchecked, k2) => {
                  k2(e.unit[R](h2.state, a))
                }
              }
            }
          }
        }
        c(HandlerCont(h, k))
      }
    }
  }
}

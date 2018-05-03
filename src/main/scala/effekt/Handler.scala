package effekt

trait Handler { h =>
  type R
  type Res
  def unit: R => Res

  def use[A](body: CPS[A, Res]): Control[A] = Control.use(this)(body)

  def handle(f: R using h.type): Control[Res] = Control.handle(this)(f)

  def apply(f: R using h.type): Control[Res] = handle(f)
}
object Handler {

  trait Basic[R0, Res0] extends Handler { outer =>
    type R = R0
    type Res = Res0
  }
}
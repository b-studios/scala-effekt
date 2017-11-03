package effekt

trait Eff extends Serializable {
  type Op[A]
}

trait Handler extends Eff { outer =>
  type R
  type Res
  type Op[A] = CPS[A, Res]
  def unit: R => Res

  def apply(f: R using outer.type): Control[Res] =
    effekt.handle(this)(f)
}
object Handler {

  trait Basic[R0, Res0] extends Handler { outer =>
    type R = R0
    type Res = Res0
  }
}
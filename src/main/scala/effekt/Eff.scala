package effekt

trait Eff extends Serializable {
  type Op[A]
}

trait Handler[R0, Res0] extends Eff {
  type R = R0
  type Res = Res0
  type Op[A] = CPS[A, Res]
  def unit: R => Res
}
object Handler {
  type Aux = Handler[_, _]
}
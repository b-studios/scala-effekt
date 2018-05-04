package effekt

trait Handler extends Prompt { h =>
  type R
  type Res
  def unit: R => Res

  def use[A](body: CPS[A, Res]): Control[A] = Control.use(this) { body }

  def handle(f: R using this.type): Control[Res] =
    Control.handle(this) { f(this) map unit }

  def apply(f: R using this.type): Control[Res] = handle(f)
}
object Handler {

  trait Basic[R0, Res0] extends Handler { outer =>
    type R = R0
    type Res = Res0
  }
}

trait Stateful[S] {
  def onSave(): S
  def onLoad(s: S): Unit
}
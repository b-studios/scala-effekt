package effekt

trait Handler extends Prompt { h =>
  type R
  type Res
  def unit: R => Control[Res]

  def use[A](body: CPS[A, Res]): Control[A] = Control.use(this) { body }

  def handle(f: R using this.type): Control[Res] =
    Control.handle(this) { (f given this) flatMap unit }

  def apply(f: R using this.type): Control[Res] = handle(f)
}

trait StatefulHandler[S] extends Handler with Stateful[S] {

  // on stateful handlers
  //   use(k -> ... k.resume(a) ...) should be equivalent to
  //   useStateful( (k, s) -> ... k.resume(a, s) ... )
  override def use[A](body: CPS[A, Res]): Control[A] = {
    val before = get()
    Control.use(this) { given k => body given (a => { put(before); k(a) }) }
  }

  def useStateful[A](body: StatefulCPS[A, Res, S]): Control[A] = {
    val before = get()
    Control.use[A](this) { given k => (body given ((a, s) => {
      put(s); k(a)
    }))(before)}
  }

  def handle(init: S)(f: R using this.type): Control[Res] = {
    put(init)
    Control.stateful(this) { _ =>
      Control.handle(this) { (f given this) flatMap unit }
    }
  }
}

object Handler {

  trait Basic[R0, Res0] extends Handler { outer =>
    type R = R0
    type Res = Res0
  }

  trait Stateful[R0, Res0, S] extends StatefulHandler[S] {
    type R = R0
    type Res = Res0
  }
}

trait Stateful[S] {
  def get(): S
  def put(s: S): Unit

  def value: S = get()
  def value_=(s: S): Unit = put(s)
}
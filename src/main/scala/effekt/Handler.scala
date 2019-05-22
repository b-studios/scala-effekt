package effekt

trait Handler[Res] extends ContMarker[Res] { h =>

  def use[A](body: CPS[A, Res]): Control[A] = Control.use(this) { body }

  def handle(f: Res using this.type): Control[Res] =
    Control.handle(this) { f }

  def apply(f: Res using this.type): Control[Res] = handle(f)
}

trait StatefulHandler[Res, S] extends Handler[Res] with Stateful[S] {

  // on stateful handlers
  //   use(k -> ... k.resume(a) ...) should be equivalent to
  //   useStateful( (k, s) -> ... k.resume(a, s) ... )
  override def use[A](body: CPS[A, Res]): Control[A] = {
    val before = get()
    Control.use(this) { given k => body given (a => { put(before); k(a) }) }
  }

  def useStateful[A](body: given ((A, S) => Control[Res]) => S => Control[Res]): Control[A] = {
    val before = get()
    Control.use(this) { given k => (body given ((a, s) => {
      put(s); k(a)
    }))(before)}
  }

  def handle(init: S)(f: Res using this.type): Control[Res] = {
    put(init)
    Control.stateful(this) { _ =>
      Control.handle(this) {  f }
    }
  }
}

object Handler {

  trait Basic[Res] extends Handler[Res]

  trait Stateful[Res, S] extends StatefulHandler[Res, S]
}

trait Stateful[S] {
  def get(): S
  def put(s: S): Unit

  def value: S = get()
  def value_=(s: S): Unit = put(s)
}
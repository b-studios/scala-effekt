package effekt

// Effekt without effect safety
// In this API we just forget about effect types
package object unsafe {
  type Control[+A] = effekt.Control[A, Pure]

  // The usual suspects
  // ===
  final def pure[A](a: => A): Control[A] = effekt.pure(a)

  type CPS[+A, R] = given (A => R) => R
  inline def resume[A, R](a: A) given (k: A => Control[R]): Control[R] = k(a)

  final def run[A](c: Control[A]): A =
    effekt.run { c.asInstanceOf[effekt.Control[A, Pure]] }

  // delimited control
  // ===

  // Just a marker trait used by the delimcc implementation
  trait Scope[R] {
    def apply[A](body: CPS[A, Control[R]]): Control[A] = switch(body)
    def switch[A](body: CPS[A, Control[R]]): Control[A]
  }
  // XXX inline here improves type inference
  inline def Scope[R] given (s: Scope[R]): s.type = s
  inline def scope[R] given (s: Scope[R]): s.type = s

  def handle[R](prog: given (c: Scope[R]) => Control[R]): Control[R] = {
    effekt.handle { given scope =>
      prog given new Scope[R] {
        def switch[A](body: CPS[A, Control[R]]) = scope.switch[A] { given k =>
          body given (k.asInstanceOf[A => Control[R]])
        }.asInstanceOf[Control[A]]
      }
    }
  }

  // delimited dynamic state
  // ===
  trait State {
    private[unsafe] val typed: effekt.State

    def Field[T](value: T) = new Field(typed.Field(value))

    class Field[T](backing: typed.Field[T]) {
      def value: Control[T] = backing.value.asInstanceOf[Control[T]]
      def value_=(value: T): Control[Unit] = { backing.value = value }.asInstanceOf[Control[Unit]]
      def update(f: T => T): Control[Unit] = for {
        old <- value
        _   <- value_=(f(old))
      } yield ()
    }
  }
  inline def State given (s: State): s.type = s
  inline def Field[T](value: T) given (s: State) = s.Field(value)

  def region[R, FX](prog: given (s: State) => Control[R]): Control[R] =
    effekt.region { given typedState =>
      val s = new State { val typed = typedState }
      prog given s
    }
}
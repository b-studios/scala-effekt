package examples

// This is core effect, based on Lambda-down-up by Zhang and Myers 2019.
package object core extends App {

  import effekt.{
    // denoted [ T ]_{es}
    Control, pure, run, Pure,

    // called down-arrow
    handle }

  // prompts are called Label in Zhang Myers
  type Label[T, E] = effekt.Scope[T, E]


  trait IF[A, B] {
    // these are implementation details and should not be set by the user
    type lbl
    def apply(arg: A): Control[B, this.lbl]
  }

  // this is like a dependent product. impl depends on `l`.
  // the singleton upper bound ensures precise type inference
  // class Handler[-A, +R, T, E](val l: Label[T, E])(
  case class Handler[A, B, T, E, L <: Label[T, E] & Singleton](
    label: L,
    handler: A => (B => Control[T, E]) => Control[T, E]
  ) extends IF[A, B] {
    type lbl = label.type
    override def apply(arg: A): Control[B, this.lbl] = label.switch { k => handler(arg)(k) }
  }
  def down[R, E](prog: (l: Label[R, E]) => Control[R, l.type & E]): Control[R, E] = handle(prog)
  def up[A, B](f: IF[A, B]): A => Control[B, f.lbl] = a => f.apply(a)


  trait Amb extends IF[Unit, Boolean]
  trait Exc extends IF[String, Nothing]

  def collect[R, E](prog: (amb: Amb) => Control[R, amb.lbl & E]): Control[List[R], E] = down { l =>
    // sadly, extending Amb does not help to infer A and B
    // but we don't care so much for type inference in this implementation, since it is only a
    // core language.
    val amb = new Handler[Unit, Boolean, List[R], E, l.type](l, unit => k => for {
      ts <- k(true)
      fs <- k(false)
    } yield ts ++ fs) with Amb
    prog(amb) map { r => List(r) }
  }

  def maybe[R, E](prog: (exc: Exc) => Control[R, exc.lbl & E]): Control[Option[R], E] = down { l =>
    val exc = new Handler[String, Nothing, Option[R], E, l.type](l, msg => k => pure(None)) with Exc
    prog(exc) map { r => Some(r) }
  }

  val result = collect { flip =>
    maybe[String, flip.lbl] { raise =>
      for {
        b <- up(flip)(())
        r <- if (b) up(flip)(()) else up(raise)("dropped")
      } yield if (r) "heads" else "tails"
    }
  }

  println { run { result } }
}

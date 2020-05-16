package examples

import effekt._
import nondet._

object parser extends App {
  trait Input extends Eff { def read(): Char / effect }
  def Input(using i: Input): i.type = i

  def parseAB(amb: Amb, exc: Exc, in: Input) : Int / (amb.effect & exc.effect & in.effect) =
    alternative(
      accept('a')(in, exc) andThen parseAB(amb, exc, in) map { _ + 1 },
      accept('b')(in, exc) map { x => 0 })(amb)

  def accept(exp: Char)(in: Input, exc: Exc) = in.read() flatMap { t =>
    if (t == exp) pure(()) else exc.raise("Expected " + exp)
  }

  def alternative[A, E](fst: A / E, snd: A / E)(amb: Amb) =
    amb.flip() flatMap { b => if (b) fst else snd }

  object implicits {
    def parseAB(using amb: Amb, exc: Exc, in: Input) : Int / (amb.effect & exc.effect & in.effect) =
      alternative(
        accept('a') andThen parseAB map { _ + 1 },
        accept('b') map { x => 0 })

    def accept(exp: Char)(using Input, Exc) = Input.read() flatMap { t =>
      if (t == exp) pure(()) else Exc.raise("Expected " + exp)
    }

    def alternative[A, E](fst: A / E, snd: A / E)(using Amb) =
      Amb.flip() flatMap { b => if (b) fst else snd }
  }

  object composition {
    trait Parser { val amb: Amb; val exc: Exc; val in: Input }

    def parseAB(p: Parser) : Int / (p.amb.effect & p.exc.effect & p.in.effect) =
      alternative(
        accept('a')(p) andThen parseAB(p) map { _ + 1 },
        accept('b')(p) map { x => 0 })(p)

    def accept(exp: Char)(p: Parser) = p.in.read() flatMap { t =>
      if (t == exp) pure(()) else p.exc.raise("Expected " + exp)
    }

    def alternative[A, E](fst: A / E, snd: A / E)(p: Parser) =
      p.amb.flip() flatMap { b => if (b) fst else snd }
    }
}
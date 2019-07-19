package effekt
package examples

object parsers extends App {

  implicit def lift[R](idiom: Idiom[R]): Eff[R] = embed(idiom)

  type Token = String

  sealed trait Parser[R] extends Op[R]
  case class Alt[R](l: Idiom[R], r: Idiom[R]) extends Op[R]
  case class Fail[A](msg: String) extends Op[A]
  case class Accept(s: Token) extends Op[Token]

  def p = !Alt(!Accept("a"), !Accept("b"))

  def p2 = p flatMap { x => !Fail("no...") }

  def printGrammar[X]: Idiom[X] => String = _.fold("") {
    case Fail(msg) => _ => "fail"
    case Accept(s) => _ => s
    case Alt(l, r) => _ => s"(${printGrammar(l)}) || (${printGrammar(r)})"
  }

  println(printGrammar(p))

  // print idiomatic subprograms
  class PrintIdioms extends DynamicHandler[String, [X] =>> String] {
    def handle[X] = prog => pure(printGrammar(prog))
    def sequence[X] = r => resume => pure(s"($r) -> <function>")
  }

  println { run {
      new PrintIdioms() apply {
        p2 map { _ => "nothing" }
      }
    }
  }
}
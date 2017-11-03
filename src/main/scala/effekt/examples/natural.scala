package effekt
package examples

trait NominalPhrase
trait Sentence

case class Person(name: String) extends NominalPhrase
case class Man(p: NominalPhrase) extends Sentence
case class Woman(p: NominalPhrase) extends Sentence

case class Love(p: NominalPhrase, target: NominalPhrase) extends Sentence

case class BestFriend(x: NominalPhrase, friend: NominalPhrase) extends Sentence

case class Say(p: NominalPhrase, sentence: Sentence) extends Sentence

case class ForAll(individual: Var, a: Sentence) extends Sentence
case class Exists(individual: Var, a: Sentence) extends Sentence
case class Implies(a: Sentence, b: Sentence) extends Sentence
case class And(a: Sentence, b: Sentence) extends Sentence
case class Or(a: Sentence, b: Sentence) extends Sentence
case class Not(s: Sentence) extends Sentence
case class Equals(a: NominalPhrase, b: NominalPhrase) extends Sentence



case class Var(x: String) extends NominalPhrase
object Var {
  // TODO use state effect
  private var _x = 0
  def fresh = {
    _x += 1
    Var(s"x${_x}")
  }
}

// Effects

trait Id[R] extends Handler.Basic[R, R] {
  def unit = a => a
}


// First effect: The contextual speaker
trait Speaker extends Eff {
  def speaker(): Op[NominalPhrase]
}
object Speaker {
  def apply[R](p: NominalPhrase)(f: R using Speaker) =
    handle(new Speaker with Id[R] {
      def speaker() = resume(p)
    })(f)

  def me: NominalPhrase using Speaker =
    implicit s => use(s)(s.effect.speaker())
}

// Second effect: Scoped sentences
trait Scope[R] extends Eff {
  def scope[A](k: CPS[A, R]): Op[A]
}
object Scope {
  def apply[R](f: R using Scope[R]): Control[R] =
    handle(new Scope[R] with Id[R] {
      def scope[A](k: CPS[A, R]) = implicit k2 => k(a => k2(a))
    })(f)

  def scope[A, R](k: CPS[A, R]): A using Scope[R] =
    implicit s => use(s)(s.effect.scope[A](k))
}

// Third effect: Conventional implicature
trait Implicature extends Eff {
  def implicate(s: Sentence): Op[Unit]
}
object Implicature {
  def accommodate(f: Sentence using Implicature) =
    handle(new Implicature with Id[Sentence] {
      def implicate(s: Sentence) = resume(()).map { x => And(s, x) }
    })(f)

  def implicate(s: Sentence): Unit using Implicature =
    implicit i => use(i)(i.effect.implicate(s))
}


object fluent extends App {
  import syntax._
  import Speaker._
  import Implicature._

  val pete = Person("Pete")
  val john = Person("John")
  val mary = Person("Mary")

  val stmt1 = john loves mary
  //> Love(Person(John),Person(Mary))

  val stmt2: Sentence using Speaker = mary loves me
  //> Love(Person(Mary),Person(Pete))

  val stmt3: Sentence using Speaker = john said { mary loves me }
  //> Say(Person(John),Love(Person(Mary),Person(Pete)))

  val stmt4: Control[Sentence] = john said quote { mary loves me }
  //> Say(Person(John),Love(Person(Mary),Person(John)))

  val stmt5: Control[Sentence] = Scope { every(Man) loves mary }
  //> ForAll(Var(x1),Implies(Man(Var(x1)),Love(Var(x1),Person(Mary))))

  val stmt6: Control[Sentence] = Scope { john said quote { every(Woman) loves me } }
  //> ForAll(Var(x2),Implies(Woman(Var(x2)),Say(Person(John),Love(Var(x2),Person(John)))))

  val stmt7: Control[Sentence] = john said quote { Scope { every(Woman) loves me } }
  //> Say(Person(John),ForAll(Var(x3),Implies(Woman(Var(x3)),Love(Var(x3),Person(John)))))

  val stmt8: Sentence using Speaker = john said Scope[Sentence] { every(Woman) loves me }
  //> Say(Person(John),ForAll(Var(x4),Implies(Woman(Var(x4)),Love(Var(x4),Person(Pete)))))

  val stmt9: Sentence using Speaker = Scope {
    accommodate {
      (john appos myBestFriend) loves every(Woman)
    }
  }
  //> ForAll(Var(x5), Implies(
  //    Woman(Var(x5)),
  //    And( BestFriend(Person(Pete), Person(John)),
  //         Love(Person(John),Var(x5)))))

  println(run { stmt1 })
  println(run { Speaker(pete) { stmt2 } })
  println(run { Speaker(pete) { stmt3 } })
  println(run { Speaker(pete) { stmt4 } })
  println(run { stmt5 })
  println(run { stmt6 })
  println(run { stmt7 })
  println(run { Speaker(pete) { stmt8 } })
  println(run { Speaker(pete) { stmt9 } })

  def myBestFriend: (NominalPhrase => Sentence) using Speaker =
    me.map { speaker => x => BestFriend(speaker, x) }
}

object syntax {


  import Implicature._

  implicit class LiftedPersonOps(p: Control[NominalPhrase]) {
    def loves(other: Control[NominalPhrase]) = for {
      first <- p
      second <- other
    } yield Love(first, second)

    def said(t: Control[Sentence]) = for {
      speaker  <- p
      sentence <- t
    } yield Say(speaker, sentence)

    // In Dotty we could use implicit function types here
    def said(t: Cap[Speaker] => Control[Sentence]) = for {
      s <- p
      sentence <- Speaker(s) { t(implicitly) }
    } yield Say(s, sentence)

    def appos(f: Control[NominalPhrase => Sentence]): NominalPhrase using Implicature =
      for {
        pred <- f
        person <- p
        _ <- implicate(pred(person))
      } yield person
  }
  implicit def lift(t: NominalPhrase): Control[NominalPhrase] = pure(t)
  implicit def autoLift(t: NominalPhrase): LiftedPersonOps = pure(t)

  //  @inline // crashes the dotty compiler
  def quote(f: Sentence using Speaker): Cap[Speaker] => Control[Sentence] = f

  def every(pred: NominalPhrase => Sentence): NominalPhrase using Scope[Sentence]
    = every(pure(pred))
  def a(pred: NominalPhrase => Sentence): NominalPhrase using Scope[Sentence]
    = a(pure(pred))

  import Scope._
  def every(pred: Control[NominalPhrase => Sentence]): NominalPhrase using Scope[Sentence]
    = scope[NominalPhrase, Sentence] {
      val x = Var.fresh
      for { p <- pred; y <- resume(x) } yield ForAll(x, Implies(p(x), y))
    }
  def a(pred: Control[NominalPhrase => Sentence]): NominalPhrase using Scope[Sentence]
    = scope[NominalPhrase, Sentence] {
      val x = Var.fresh
      for { p <- pred; y <- resume(x) } yield Exists(x, And(p(x), y))
    }
}
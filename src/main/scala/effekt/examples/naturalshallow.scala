package effekt
package examples
package shallow

trait Sentences[NP, S] {
  def person(name: String): Control[NP]

  def man(person: NP): Control[S]
  def woman(person: NP): Control[S]

  def love(person: NP, target: NP): Control[S]
  def bestFriend(person: NP, friend: NP): Control[S]
  def say(person: NP, sentence: S): Control[S]
}

trait Logical[Individual, Stmt] {
  def forall(f: Individual => Control[Stmt]): Control[Stmt]
  def exists(f: Individual => Control[Stmt]): Control[Stmt]

  def implies(first: Stmt, second: Stmt): Control[Stmt]
  def and(first: Stmt, second: Stmt): Control[Stmt]
}

trait Syntax { self: SpeakerDSL with ScopeDSL with ImplicatureDSL =>

  type NP
  type S

  def person(name: String)(implicit alg: Sentences[NP, _]): Control[NP] = alg.person(name)

  def man(implicit alg: Sentences[NP, S]): NP => Control[S] = p => alg.man(p)
  def woman(implicit alg: Sentences[NP, S]): NP => Control[S] = p => alg.woman(p)

  def love(person: NP, target: NP)(implicit alg: Sentences[NP, S]): Control[S] = alg.love(person, target)
  def bestFriend(person: NP, friend: NP)(implicit alg: Sentences[NP, S]): Control[S] = alg.bestFriend(person, friend)
  def say(person: NP, sentence: S)(implicit alg: Sentences[NP, S]): Control[S] = alg.say(person, sentence)


  implicit class LiftedPersonOps(p: Control[NP])(implicit alg: Sentences[NP, S]) {
    def loves(other: Control[NP]) = for {
      first  <- p
      second <- other
      res    <- love(first, second)
    } yield res

    def said(t: Control[S]) = for {
      speaker  <- p
      sentence <- t
      res      <- say(speaker, sentence)
    } yield res

    def said(t: Cap[Speaker] => Control[S]) = for {
      s        <- p
      sentence <- withSpeaker(s) { t(implicitly) }
      res      <- say(s, sentence)
    } yield res

    def who(f: NP => Control[S]): NP using Implicature =
      for {
        person <- p
        y      <- f(person)
        _      <- imply(y)
      } yield person
  }

  def quote(f: S using Speaker): Cap[Speaker] => Control[S] = f

  implicit def lift(t: NP)(implicit alg: Sentences[NP, _]): Control[NP] =
    pure(t)

  implicit def liftOps(t: NP)(implicit alg: Sentences[NP, S]): LiftedPersonOps =
    new LiftedPersonOps(pure(t))

  def every(pred: NP => Control[S])(implicit alg: Logical[NP, S]): NP using Scope[S] =
    scope[NP, S] {
      alg.forall { x => for {
          p   <- pred(x)
          y   <- resume(x)
          res <- alg.implies(p, y)
        } yield res
      }
    }

  def a(pred: NP => Control[S])(implicit alg: Logical[NP, S]): NP using Scope[S] =
    scope[NP, S] {
      alg.exists { x => for {
          p   <- pred(x)
          y   <- resume(x)
          res <- alg.and(p, y)
        } yield res
      }
    }
}

// Effects

//
//
//// First effect: The contextual speaker
trait SpeakerDSL {

  type NP
  type S

  trait Speaker extends Eff {
    def speaker(): Op[NP]
  }

  def withSpeaker[R](p: NP)(f: R using Speaker) =
    handle(new Speaker with Id[R] {
      def speaker() = resume(p)
    })(f)

  def me: NP using Speaker =
    implicit s => use(s)(s.effect.speaker())
}

trait ScopeDSL {

  trait Scope[R] extends Eff {
    def scope[A](k: CPS[A, R]): Op[A]
  }

  def scoped[R](f: R using Scope[R]): Control[R] =
    handle(new Scope[R] with Id[R] {
      def scope[A](k: CPS[A, R]) = implicit k2 => k(a => k2(a))
    })(f)

  def scope[A, R](k: CPS[A, R]): A using Scope[R] =
    implicit s => use(s)(s.effect.scope[A](k))
}

// Third effect: Conventional implicature
trait ImplicatureDSL {

  type S

  trait Implicature extends Eff {
    def implicate(s: S): Op[Unit]
  }

  def accommodate(f: S using Implicature)(implicit alg: Logical[_, S]) =
    handle(new Implicature with Id[S] {
      def implicate(s: S) = resume(()) flatMap { x => alg.and(s, x) }
    })(f)

  def imply(s: S): Unit using Implicature =
    implicit i => use(i)(i.effect.implicate(s))

}


trait Statements extends Syntax with SpeakerDSL with ScopeDSL with ImplicatureDSL {

  type NP
  type S

  implicit def semantics: Sentences[NP, S] with Logical[NP, S]

  val pete = person("Pete")
  val john = person("John")
  val mary = person("Mary")

  val stmt1 = john loves mary
  //> Love(Person(John),Person(Mary))

  val stmt2: S using Speaker = mary loves me
  //> Love(Person(Mary),Person(Pete))

  val stmt3: S using Speaker = john said { mary loves me }
  //> Say(Person(John),Love(Person(Mary),Person(Pete)))

  val stmt4: Control[S] = john said quote { mary loves me }
  //> Say(Person(John),Love(Person(Mary),Person(John)))

  val stmt5: Control[S] = scoped { every(man) loves mary }
  //> ForAll(Var(x1),Implies(Man(Var(x1)),Love(Var(x1),Person(Mary))))

  val stmt6: Control[S] = scoped { john said quote { every(woman) loves me } }
  //> ForAll(Var(x2),Implies(Woman(Var(x2)),Say(Person(John),Love(Var(x2),Person(John)))))

  val stmt7: Control[S] = john said quote { scoped { every(woman) loves me } }
  //> Say(Person(John),ForAll(Var(x3),Implies(Woman(Var(x3)),Love(Var(x3),Person(John))))

  val stmt8: S using Speaker = john said scoped[S] { every(woman) loves me }
  //> Say(Person(John),ForAll(Var(x4),Implies(Woman(Var(x4)),Love(Var(x4),Person(Pete)))))


  def isMyBestFriend: implicit Cap[Speaker] => (NP => Control[S]) =
    other => me flatMap { i => bestFriend(i, other) }

  val stmt9: S using Speaker = scoped {
    accommodate {
      (john who isMyBestFriend) loves every(woman)
    }
  }
  //> ForAll(Var(x5), Implies(
  //    Woman(Var(x5)),
  //    And( BestFriend(Person(Pete), Person(John)),
  //         Love(Person(John),Var(x5)))))


  // Run the examples
  println(run { stmt1 })
  println(run { pete said quote(stmt2) })
  println(run { pete said quote(stmt3) })
  println(run { pete said quote(stmt4) })
  println(run { stmt5 })
  println(run { stmt6 })
  println(run { stmt7 })
  println(run { pete said quote(stmt8) })
  println(run { pete said quote(stmt9) })
}

object ReifySentences extends Sentences[NominalPhrase, Sentence] with Logical[NominalPhrase, Sentence] {
  def person(name: String) = pure(Person(name))

  def man(person: NominalPhrase) = pure(Man(person))
  def woman(person: NominalPhrase) = pure(Woman(person))

  def love(person: NominalPhrase, target: NominalPhrase) = pure(Love(person, target))
  def bestFriend(person: NominalPhrase, friend: NominalPhrase) = pure(BestFriend(person, friend))
  def say(person: NominalPhrase, sentence: Sentence) = pure(Say(person, sentence))

  def forall(f: NominalPhrase => Control[Sentence]): Control[Sentence] = {
    val x = Var.fresh
    f(x).map { y => ForAll(x, y) }
  }
  def exists(f: NominalPhrase => Control[Sentence]): Control[Sentence] = {
    val x = Var.fresh
    f(x).map { y => Exists(x, y) }
  }

  def implies(first: Sentence, second: Sentence) = pure(Implies(first, second))
  def and(first: Sentence, second: Sentence) = pure(And(first, second))
}

object fluent extends Statements with App {
  type NP = NominalPhrase
  type S = Sentence

  lazy val semantics = ReifySentences
}
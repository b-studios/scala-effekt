// package effekt
// package examples
// package shallow

// import scala.language.implicitConversions

// trait Sentences[NP, S] {
//   def person(name: String): Control[NP]

//   def man(person: NP): Control[S]
//   def woman(person: NP): Control[S]

//   def love(person: NP, target: NP): Control[S]
//   def bestFriend(person: NP, friend: NP): Control[S]
//   def say(person: NP, sentence: S): Control[S]
// }

// trait Logical[Individual, Stmt] {
//   def forall(f: Individual => Control[Stmt]): Control[Stmt]
//   def exists(f: Individual => Control[Stmt]): Control[Stmt]

//   def implies(first: Stmt, second: Stmt): Control[Stmt]
//   def and(first: Stmt, second: Stmt): Control[Stmt]
//   def or(first: Stmt, second: Stmt): Control[Stmt]
//   def not(s: Stmt): Control[Stmt]
//   def equals(first: Individual, second: Individual): Control[Stmt]
// }

// trait Syntax { self: SpeakerDSL with ScopeDSL with ImplicatureDSL with SubjectDSL with FocusDSL =>

//   type NP
//   type S

//   def person(name: String)(using alg: Sentences[NP, _]): Control[NP] = alg.person(name)

//   def man(using alg: Sentences[NP, S]): NP => Control[S] = p => alg.man(p)
//   def woman(using alg: Sentences[NP, S]): NP => Control[S] = p => alg.woman(p)

//   def love(person: Control[NP], target: Control[NP])(using alg: Sentences[NP, S]): Control[S] = for {
//     x   <- person
//     y   <- target
//     res <- alg.love(x, y)
//   } yield res

//   def bestFriend(person: Control[NP], friend: Control[NP])(using alg: Sentences[NP, S]): Control[S] = for {
//     p   <- person
//     f   <- friend
//     res <- alg.bestFriend(p, f)
//   } yield res

//   def say(person: Control[NP], sentence: Control[S])(using alg: Sentences[NP, S]): Control[S] = for {
//     p   <- person
//     s   <- sentence
//     res <- alg.say(p, s)
//   } yield res

//   def and(first: Control[S], second: Control[S])(using alg: Logical[NP, S]): Control[S] = for {
//       x   <- first
//       y   <- second
//       res <- alg.and(x, y)
//     } yield res

//   def or(first: Control[S], second: Control[S])(using alg: Logical[NP, S]): Control[S] = for {
//       x   <- first
//       y   <- second
//       res <- alg.or(x, y)
//     } yield res

//   def equals(first: Control[NP], second: Control[NP])(using alg: Logical[NP, S]): Control[S] = for {
//       x   <- first
//       y   <- second
//       res <- alg.equals(x, y)
//     } yield res

//   implicit class LiftedPersonOps(p: Control[NP])(using alg: Sentences[NP, S]) {
//     def loves(other: Control[NP]) = self.love(p, other)
//     def love(other: Control[NP]) = self.love(p, other)

//     def said(s: Control[S]) = say(p, s)

//     def said(t: Speaker => Control[S]) = say(p, withSpeaker(p) { t(implicitly) })

//     def bestFriendOf(other: Control[NP]) = bestFriend(other, p)

//     def whoIs(f: S using Subject): NP using Implicature =
//       for {
//         person <- p
//         y      <- withSubject(person) { f }
//         _      <- imply(y)
//       } yield person

//     def focusing(f: S using Subject and Focus): S using Logical[NP, S] = for {
//       person <- p
//       y      <- withSubject(person) { withFocus { f } }
//     } yield y

//     // now combine whoIs and focusing
//     def who(f: S using Subject and Focus): NP using Implicature and Logical[NP, S] = for {
//       person <- p
//       y      <- withSubject(person) { withFocus { f } }
//       _      <- imply(y)
//     } yield person

//   }

//   implicit class LiftedSentenceOps(s: Control[S]) {
//     def and(other: Control[S]): S using Logical[NP, S] = self.and(s, other)
//   }

//   def quote(f: S using Speaker): Speaker => Control[S] = s => f(using s)

//   implicit def liftOps(t: NP)(using alg: Sentences[NP, S]): LiftedPersonOps =
//     new LiftedPersonOps(pure(t))

//   def every(pred: NP => Control[S])(using alg: Logical[NP, S]): NP using Scope[S] =
//     scope[NP, S] {
//       alg.forall { x => for {
//           p   <- pred(x)
//           y   <- resume(x)
//           res <- alg.implies(p, y)
//         } yield res
//       }
//     }

//   def a(pred: NP => Control[S])(using alg: Logical[NP, S]): NP using Scope[S] =
//     scope[NP, S] {
//       alg.exists { x => for {
//           p   <- pred(x)
//           y   <- resume(x)
//           res <- alg.and(p, y)
//         } yield res
//       }
//     }
// }

// // Effects

// trait SpeakerDSL {
//   type NP
//   type S

//   type Speaker

//   def withSpeaker[R](p: NP)(f: R using Speaker): Control[R]
//   def withSpeaker[R](p: Control[NP])(f: R using Speaker): Control[R]
//   def me: NP using Speaker
// }

// //
// //
// //// First effect: The contextual speaker
// trait SpeakerEffectDSL extends SpeakerDSL {
//   @annotation.implicitNotFound("Cannot find a speaker, which is required here.\nSpeakers can be introduced using `p said quote { s }` or using `withSpeaker(p) { s }`")
//   trait Speaker {
//     def speaker(): Control[NP]
//   }

//   def withSpeaker[R](p: NP)(f: R using Speaker): Control[R] = f(using { () => pure(p) })

//   def withSpeaker[R](p: Control[NP])(f: R using Speaker): Control[R] = p flatMap { p => f(using { () => pure(p) }) }

//   def me: NP using Speaker = s ?=> s.speaker()
//   def I: NP using Speaker = me
// }

// trait ScopeDSL {

//   @annotation.implicitNotFound("This expression is scoped and requires a delimiter with type ${R}.\nDelimiters can be introduced using `scoped[${R}] { ... }`")
//   trait Scope[R] {
//     def scope[A](k: CPS[A, R]): Control[A]
//   }

//   def scoped[R](f: R using Scope[R]): Control[R] =
//     handle(new Scope[R] with Id[R] {
//       def scope[A](k: CPS[A, R]) = use { k(using resume) }
//     })(f)

//   def scope[A, R](k: CPS[A, R]): A using Scope[R] = s ?=> s.scope[A](k)
// }

// // Third effect: Conventional implicature
// trait ImplicatureDSL {

//   type S

//   @annotation.implicitNotFound("This expression needs to be accommodated.\nThis can be achieved by wrapping it into a call to `accommodate` or by bringing an implicit\ninstance of Implicature into scope.")
//   trait Implicature {
//     def implicate(s: S): Control[Unit]
//   }

//   def accommodate(f: S using Implicature)(using alg: Logical[_, S]) =
//     handle(new Implicature with Id[S] {
//       def implicate(s: S) = use { resume(()) flatMap { x => alg.and(s, x) } }
//     })(f)

//   def imply(s: S): Unit using Implicature = i ?=> i.implicate(s)

// }

// // another anaphoric reference
// // XXX ask Julian whether subject makes sense here
// trait SubjectDSL {

//   type NP
//   type S

//   @annotation.implicitNotFound("Cannot find a subject, which is required here.")
//   case class Subject(person: NP)

//   def withSubject[R](p: NP)(f: R using Subject): Control[R] = withSubject(pure(p))(f)

//   def withSubject[R](p: Control[NP])(f: R using Subject): Control[R] =
//     p flatMap { person => f(using Subject(person)) }

//   def he: NP using Subject = s ?=> pure(s.person)
//   def she: NP using Subject = s ?=> pure(s.person)
// }

// trait FocusDSL {

//   type NP
//   type S

//   // Our universal quantification is not general enough to focus on
//   // arbitrary particles in the sentence.
//   // So for now we implement focusing on NPs

//   // handler for focus
//   // XXX change to use Syntax instead!
//   def withFocus(f: S using Focus)(using alg: Logical[NP, S]) =
//     handle(new Focus with Id[S] {
//       def focus(p: NP) = use {
//         for {
//           x <- resume(p)
//           y <- alg forall { other =>
//             for {
//               s1 <- resume(other)
//               eq <- alg.equals(p, other)
//               neg <- alg.not(s1)
//               s2 <- alg.or(eq, neg)
//             } yield s2
//           }
//           res <- alg.and(x, y)
//         } yield res
//       }
//     })(f)

//   //
//   trait Focus  {
//     def focus(a: NP): Control[NP]
//   }
//   def only(p: Control[NP]): NP using Focus = f ?=> p.flatMap { f.focus }

// }

// trait Statements extends Syntax with SpeakerEffectDSL with ScopeDSL with ImplicatureDSL with SubjectDSL with FocusDSL {

//   type NP
//   type S

//   def semantics: Sentences[NP, S] & Logical[NP, S]
//   given sem : (Sentences[NP, S] & Logical[NP, S]) = semantics

//   val pete = person("Pete")
//   val john = person("John")
//   val mary = person("Mary")

//   val stmt1 = john loves mary
//   //> Love(Person(John),Person(Mary))

//   val stmt2: S using Speaker = mary loves me
//   //> Love(Person(Mary),Person(Pete))

//   val stmt3: S using Speaker = john said { mary loves me }
//   //> Say(Person(John),Love(Person(Mary),Person(Pete)))

//   val stmt4: Control[S] = john said quote { mary loves me }
//   //> Say(Person(John),Love(Person(Mary),Person(John)))

//   val stmt5: Control[S] = scoped { every(man) loves mary }
//   //> ForAll(Var(x1),Implies(Man(Var(x1)),Love(Var(x1),Person(Mary))))

//   val stmt6: Control[S] = scoped { john said quote { every(woman) loves me } }
//   //> ForAll(Var(x2),Implies(Woman(Var(x2)),Say(Person(John),Love(Var(x2),Person(John)))))

//   val stmt7: Control[S] = john said quote { scoped { every(woman) loves me } }
//   //> Say(Person(John),ForAll(Var(x3),Implies(Woman(Var(x3)),Love(Var(x3),Person(John))))

//   val stmt8: S using Speaker = john said scoped[S] { every(woman) loves me }
//   //> Say(Person(John),ForAll(Var(x4),Implies(Woman(Var(x4)),Love(Var(x4),Person(Pete)))))


//   def myBestFriend: S using Speaker and Subject = bestFriend(me, he)

//   val stmt9: S using Speaker = scoped {
//      accommodate {
//       john whoIs { he bestFriendOf me } loves every { woman }
//     }
//   }
//   //> ForAll(Var(x5), Implies(
//   //    Woman(Var(x5)),
//   //    And( BestFriend(Person(Pete), Person(John)),
//   //         Love(Person(John),Var(x5)))))

//   val stmt10: S using Speaker = mary focusing { she loves only(me) }
//   //> And(Love(Person(Mary), Person(Pete)),
//   //    ForAll(Var(x6),
//   //      Or(Equals(Person(Pete),Var(x6)),
//   //      Not(Love(Person(Mary),Var(x6))))))

//   val stmt11: S using Speaker = mary focusing { only(she) loves me }
//   //> And(Love(Person(Mary), Person(Pete)),
//   //    ForAll(Var(x7),
//   //      Or(Equals(Person(Mary),Var(x7)),
//   //      Not(Love(Var(x7),Person(Pete))))))


//   val stmt12: S using Speaker = accommodate {
//     mary who { she loves only(me) } said quote { I love john }
//   }
//   //> And(
//   //    And(Love(Person(Mary), Person(Pete)),
//   //        ForAll(Var(x8),
//   //          Or(Equals(Person(Pete), Var(x8)),
//   //             Not(Love(Person(Mary), Var(x8)))))),
//   //    And(And(Love(Person(Mary), Person(Pete)),
//   //            ForAll(Var(x9),
//   //              Or(Equals(Person(Pete),Var(x9)),
//   //                 Not(Love(Person(Mary),Var(x9)))))),
//   //        Say(Person(Mary), Love(Person(Mary), Person(John)))))

//   // Run the examples
//   println(run { stmt1 })
//   println(run { pete said quote(stmt2) })
//   println(run { pete said quote(stmt3) })
//   println(run { pete said quote(stmt4) })
//   println(run { stmt5 })
//   println(run { stmt6 })
//   println(run { stmt7 })
//   println(run { pete said quote(stmt8) })
//   println(run { pete said quote(stmt9) })
//   println(run { pete said quote(stmt10) })
//   println(run { pete said quote(stmt11) })
//   println(run { pete said quote(stmt12) })
// }

// object ReifySentences extends Sentences[NominalPhrase, Sentence] with Logical[NominalPhrase, Sentence] {
//   def person(name: String) = pure(Person(name))

//   def man(person: NominalPhrase) = pure(Man(person))
//   def woman(person: NominalPhrase) = pure(Woman(person))

//   def love(person: NominalPhrase, target: NominalPhrase) = pure(Love(person, target))
//   def bestFriend(person: NominalPhrase, friend: NominalPhrase) = pure(BestFriend(person, friend))
//   def say(person: NominalPhrase, sentence: Sentence) = pure(Say(person, sentence))

//   def forall(f: NominalPhrase => Control[Sentence]): Control[Sentence] = {
//     val x = Var.fresh
//     f(x).map { y => ForAll(x, y) }
//   }
//   def exists(f: NominalPhrase => Control[Sentence]): Control[Sentence] = {
//     val x = Var.fresh
//     f(x).map { y => Exists(x, y) }
//   }

//   def implies(first: Sentence, second: Sentence) = pure(Implies(first, second))
//   def and(first: Sentence, second: Sentence) = pure(And(first, second))
//   def or(first: Sentence, second: Sentence) = pure(Or(first, second))
//   def not(s: Sentence) = pure(Not(s))
//   def equals(first: NominalPhrase, second: NominalPhrase) = pure(Equals(first, second))
// }

// object fluent extends Statements with App {
//   type NP = NominalPhrase
//   type S = Sentence

//   lazy val semantics = ReifySentences
// }
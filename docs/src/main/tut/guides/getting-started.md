---
layout: docs
title:  "Getting Started"
section: "guides"
---

# Getting Started
Before you can get started with **Effekt** you need to set up your
environment and add **Effekt** as a dependency to your `build.sbt` file:

```
libraryDependencies += "de.b-studios" %% "effekt" % "0.1-SNAPSHOT"
```

Alternatively, to play around with **Effekt** in your browser, you can
also use Scastie. We prepared two different Scasties for you

- [Scala 2.12](https://scastie.scala-lang.org/3pVyLtGKTeKCisNHZAtLXw)
- [Dotty](https://scastie.scala-lang.org/pS36lNr6SPmjm4iwcyR4vw) -- Here you can also play around with implicit function types.

## Define your first effect
First we import all the necessary types and functions.
Then, to define our first effect, we specify the *effect signature*.

```tut:book:silent
import effekt._

// The effect signature
trait Amb extends Eff { self =>
  def flip[R](): Boolean @@ R
}
```

Effect signatures in **Effekt** are ordinary Scala traits that extend
from the library-trait `Eff`. The trait `Eff` has some abstract members
which we will encounter when we implement a handler for our effect. It
also comes with a few handy methods and type aliases that help us
define our effect signatures and handlers.

For effect signatures the most important type alias one is `A @@ R`
(that is `@@[A, R]` written as infix type constructor). The desugaring
of the type alias is given below. For now we only have to know that all
*effect operations* like `flip` have to carry an additional type
parameter `R`. That is, we can read `flip` as a non- deterministic
coin-flipping operation that will locally return a `Boolean` to the
caller, but the overall result of all computation following `flip` will
result into something that contains an `R`.

## Using the `Amb` effect

To actually use the flip effect we need to get our hands on a
capability that entitles us to do so. Since we don't yet know where to
get such capabilities from, we just define a function asking for it.
We flip a coin once and then return 2 or 3 depending on the result.

The type of the function argument `Use[Amb]` tells us, that we are
allowed to use the `Amb`-effect.

```tut:book:silent
def prog1(amb: Use[Amb]): Control[Int] =
  use(amb)(amb.effect.flip()) map { x => if (x) 2 else 3 }
```

The result type `Control[Int]` tells us that the integer-result will be
contained in the `Control` *monad* which is **Effekt** specific. The
call to `use` shows us that we use the capability `Use[Amb]` in two ways:

1. It serves as a proof, that we are allowed to use effect operations
   defined in the `Amb` signature.
2. It carries the effect implementation (handler code) as member `amb.effect`.

The `flip` method in the effect signature will turn out to be
very convenient for handler implementations. However, writing
`use(u)(u.effect.op())` for every use of `op` can be tiresome. Hence,
we establish the convention to define a second variant for each effect
operation as part of the effect signature's companion object:

```tut:book:silent
// The effect companion object
object Amb {
  def flip()(implicit u: Use[Amb]) = use(u) { u.effect.flip() }
}
```

After importing the operations from the companion object, we can
just use it like a normal function. Note that the capability is
implicitly passed using Scala's feature of implicit arguments.

```tut:book:silent
import Amb._
def prog(implicit amb: Use[Amb]): Control[Int] =
  flip() map { x => if (x) 2 else 3 }
```

## Defining Handlers
Let us now define our own handler for the `Amb` effect. A handler is
just an implementation of the effect interface. However, it also
needs to give the type to interpret the effect into (`Out[A]`) and
a function `unit` that lifts pure values into the effect interpretation.

```tut:book:silent
object ambHandler extends Amb {
  type Out[A] = List[A]
  type State = Unit // We don't use the handler state, yet.
  def flip[R]() = s => resume =>
    for {
      t <- resume(true)(s)
      f <- resume(false)(s)
    } yield t ++ f
  def unit[A] = (s, a) => List(a)
}
```

In the handler code we now can see why we defined `flip` to return
`Boolean @@ R` before. To implement `flip` we get access to the handler
state and the continuation `resume` (the remaining program after the flip effect,
up to the handler).

```
type @@[A, R] =
  State =>                             // the handler state
  (A => State => Control[Out[R]]) =>   // the continuation
  Control[Out[R]]                      // the result type
```

## Handling Effects
To handle an effect we use the library function `handle(handler)(state)(prog)` which gives
us a capability for the effect type that `handler` implements.

```tut:book:silent
val handled: Control[List[Int]] = handle(ambHandler) { implicit h => prog }
```

After all effects are handled, we can run the effectful computation:

```tut
handled.run()
```

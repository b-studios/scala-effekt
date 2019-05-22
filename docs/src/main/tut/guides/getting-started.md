---
layout: docs
title:  "Getting Started"
section: "guides"
---

# Getting Started
Before you can get started with **Effekt** you need to set up your
environment and add **Effekt** as a dependency to your `build.sbt` file:

```
libraryDependencies += "de.b-studios" %% "effekt" % "0.4-SNAPSHOT"
```

Alternatively, to play around with **Effekt** in your browser, you can
also use Scastie. We prepared two different Scasties for you

- [Scala 2.12](https://scastie.scala-lang.org/o2IcM13UTt60YKlcyNtv7g)
- [Dotty](https://scastie.scala-lang.org/Ilonelw2QgGDXh1pg4yXkg) -- Here
  you can also play around with implicit function types. The interface
  is slightly different at the moment.

## Define your first effect
First we import all the necessary types and functions.
Then, to define our first effect, we specify the *effect signature*.

```tut:book:silent
import effekt._

// The effect signature
trait Amb {
  def flip(): Control[Boolean]
}
```

Effect signatures in **Effekt** are ordinary Scala traits that simply
declare the effect operations.

For programming with effects the most important type is `Control`.
For now we only have to know that all
*effect operations* like `flip` have to be marked as returning
something in `Control`.

## Using the `Amb` effect

To actually use the flip effect we need to get our hands on a
capability (that is, an instance of the effect signature `Amb`)
that entitles us to do so. Since we don't yet know where to
get such capabilities from, we just define a function asking for it.
We flip a coin once and then return 2 or 3 depending on the result.


```tut:book:silent
def prog(amb: Amb): Control[Int] = for {
  x <- amb.flip()
} yield if (x) 2 else 3
```

The result type `Control[Int]` tells us that the integer-result will be
contained in the `Control` *monad* which is **Effekt** specific.


## Defining Handlers
Let us now define our own handler for the `Amb` effect. A handler is
just an implementation of the effect interface. However, it also
needs to give the type to interpret the effect into (`List[R]`).
Note how we make sure that the result of the handled program is
a list by mapping `r => List(r)` over it before applying the handler.
This way we convert the result type from `R` into the semantic
domain `List[R]`.

```tut:book:silent
def ambList[R](prog: Amb => Control[R]): Control[List[R]] =
  new Handler[List[R]] with Amb {
    def flip() = use { resume => for {
       ts <- resume(true)
        fs <- resume(false)
      } yield ts ++ fs
    }
  } handle { amb => prog(amb) map { r => List(r) } }
```

Notice how we use the trait `Handler` which provides us with the
method `use`. To implement `flip` we get access to the continuation
`resume` (the remaining program after the flip effect, up to the handler).

The type of the expression passed to `use` is thus:
```
type Body = (Boolean => Control[List[R]]) => Control[List[R]
//          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
//                  the continuation
```
Note how both, the continuation and the result type, coincide
to be `Control[List[R]]`.

## Handling Effects
To handle an effect we use the apply method that is defined on an
instance of `handler`.

```tut:book:silent
val handled: Control[List[Int]] = ambList { amb => prog(amb) }
```

One way of thinking about algebraic effects and handlers is to think
as (resumable) exceptions. In our example, the effect operation `flip`
would correspond to `throw` and the handler `ambList` to a surrounding
`try { ... }`.

After all effects are handled, we can now run the effectful computation:

```tut
run { handled }
```

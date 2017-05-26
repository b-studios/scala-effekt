---
layout: page
title:  "Multiple Effects"
section: "guides"
position: 3
---

# Combining Multiple Effects with Handlers
After having defined a single effect in [Getting Started](./first-effect), this quick tutorial shows how multiple, different effects can be defined and handled in **Effekt**.

Again, we prepared Scasties to follow along with this tutorial.

- [Scala 2.12](https://scastie.scala-lang.org/14UuWNqvSvu0g4seLZ4iug) ([full solution](https://scastie.scala-lang.org/3VsqIjFNTYeQ4HBc44wEpQ))

## Defining a second effect
In [Getting Started](./first-effect) we have defined ambiguity as our
first effect. The ambiguity effect signature had one effect operation
and looked like this:

```tut:invisible
import effekt._
```

```tut:book:silent
trait Amb extends Eff {
  def flip[R](): Boolean @@ R
}
```

```tut:invisible
object Amb {
  def flip()(implicit u: Use[Amb]) = use(u) { u.effect.flip() }
}
object ambHandler extends Amb {
  type Out[A] = List[A]
  type State = Unit // We don't use the handler state, yet.
  def unit[A] = (s, a) => List(a)

  def flip[R]() = s => resume =>
    for {
      t <- resume(true)(s)
      f <- resume(false)(s)
    } yield t ++ f
}
```
We also defined a handler for the ambiguity effect as an implementation
of the `Amb` trait, called `ambHandler`. To see how to combine two
different effects, we will now first define a second (quite standard)
effect: Mutable state. As before we start with the effect signature
and the corresponding companion object:

```tut:book:silent
trait State[S] extends Eff {
  def get[R](): S @@ R
  def put[R](s: S): Unit @@ R
}
object State {
  def get[S]()(implicit u: Use[State[S]]) = use(u) { u.effect.get() }
  def put[S](s: S)(implicit u: Use[State[S]]) = use(u) { u.effect.put(s) }
}
```
Having defined the effect signature, we can implement a handler that,
like the state monad, does not actually use mutable state but passes
the current value around through the whole program. Since state is so
important and often used, in **Effekt** every handler is already
equipped with a *handler state* that is automatically passed around
through all handler calls.

We simply need to define the type of the state by instantiating the
type member `State`. Let us look again at the implementation of the
type alias `A @@ R`:

```
type @@[A, R] =
  State =>                             // the handler state
  (A => State => Control[Out[R]]) =>   // the continuation
  Control[Out[R]]                      // the result type
```

To handle an effect, we get hold to the current state as well as a
continuation which takes the return value and an updated state.
Using this definition, we can implement a state handler as:

```tut:book:silent
def stateHandler[S] = new State[S] {
  type Out[A] = A
  type State = S // we use the handler state for state passing
  def unit[A] = (s, a) => a

  def get[R]() = s => resume => resume(s)(s)
  def put[R](s: S) = _ => resume => resume(())(s)
}
```

Please note, that it is important not to define the result type of
`stateHandler` as `State[S]` since the result type is more
special (`State[S] { type Out[A] = A; type State = S }`).

## Using `Amb` and `State` in one example
Let us now use the two effects to write a program that combines them.

```tut:book:silent
import State._, Amb._

def example(implicit u1: Use[State[Int]], u2: Use[Amb]): Control[Int] = for {
  x <- get()
  b <- flip()
  _ <- if (b) put(x + 1) else pure(())
  y <- get()
} yield (x + y)
```
The example program requires capabilities for both effects, a state
effect carrying an integer and the ambiguity effect. We first retreive
the current state, then toss a coin and depending on result mutate the
state (or not).

As you can see, there is nothing special to using more than one effect.
The use of effects naturally composes.

## Handling the two effects
As with monad transformers, we have two different ways of handling the
two effects. Should we first handle away the ambiguity effect and then
consider state, or the other way around? The nice thing with algebraic
effects and handlers is that we can decide very late.

Let's experiment with the two options:

```tut:book:silent
val result1: Control[List[Int]] = handle(ambHandler) { implicit a =>
  handle(stateHandler[Int])(0) { implicit s =>
    example
  }
}
```
```tut
result1.run()
```

In this variant, we first handle away the state effect (remember `stateHandler.Out[A] = A` in our case) and then handle ambiguity (here, `ambHandler.Out[A] = List[A]`).
The ambiguity handler invokes the continuation twice, once with `true`
and once with `false`. Since the state handler is nested in the
ambiguity handler, it will be reset for the second invocation with `false`
and thus the second element in the resulting list is `0`.

Commuting the two handlers, we get different results:

```tut:book:silent
val result2 = handle(stateHandler[Int])(0) { implicit s =>
  handle(ambHandler) { implicit a =>
    example
  }
}
```
```tut
result2.run()
```

Now, ambiguity is run inside of state and the change of state carries
over to the second coin flipping result. The result type however is the
same, since `Id[List[_]] = List[Id[_]]`. Please note, that this does
not hold in general for two arbitrary effect handlers.

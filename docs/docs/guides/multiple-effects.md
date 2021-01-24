---
layout: docs
title:  "Multiple Effects"
section: "guides"
---

# Combining Multiple Effects with Handlers
After having defined a single effect in [Getting Started](./getting-started), this quick tutorial shows how multiple, different effects can be defined and handled in **Effekt**.

Again, we prepared Scasties to follow along with this tutorial.

- [Scala 2.12](https://scastie.scala-lang.org/y2xwgsrNT5iKXk0ghfbHJg) ([full solution](https://scastie.scala-lang.org/8z6r2243S02DB50zbetshQ))

## Defining a second effect
In [Getting Started](./getting-started) we have defined ambiguity as our
first effect. The ambiguity effect signature had one effect operation
and looked like this:

```scala mdoc:silent
import effekt._

trait Amb {
  def flip(): Control[Boolean]
}
```

```scala mdoc:silent
def ambList[R](prog: Amb => Control[R]): Control[List[R]] =
  new Handler[List[R]] with Amb {
    def flip() = use { resume => for {
      ts <- resume(true)
      fs <- resume(false)
    } yield ts ++ fs }
  } handle { amb => prog(amb) map { r => List(r) } }
```
We also defined a handler for the ambiguity effect as an implementation
of the `Amb` trait, called `ambList`. To see how to combine two
different effects, we will now first define a second (quite standard)
effect: Mutable state. As before we start with the effect signature.

```scala mdoc:silent
trait State[S] {
  def get(): Control[S]
  def put(s: S): Control[Unit]
}
```
Having defined the effect signature, we can implement a handler that,
like the state monad, does not actually use mutable state but passes
the current value around through the whole program.


```scala mdoc:silent
def state[R, S](init: S)(prog: State[S] => Control[R]): Control[R] =
  new Handler[S => Control[R]] with State[S] {
    def put(s: S) = use { resume => pure { s2 => resume(()) flatMap { _ apply s } } }
    def get()     = use { resume => pure { s2 => resume(s2) flatMap { _ apply s2 } } }
  } handle { state => prog(state) map { r => s => pure(r) } } flatMap { _ apply init }
```

## Using `Amb` and `State` in one example
Let us now use the two effects to write a program that combines them.

```scala mdoc:silent

def example(implicit s: State[Int], amb: Amb): Control[Int] = for {
  x <- s.get()
  b <- amb.flip()
  _ <- if (b) s.put(x + 1) else pure(())
  y <- s.get()
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

```scala mdoc:silent
val result1: Control[List[Int]] = ambList { implicit a: Amb =>
  state(0) { implicit s: State[Int] =>
    example
  }
}
```
```scala mdoc
result1.run()
```

In this variant, we first handle away the state effect and then handle ambiguity.
The ambiguity handler invokes the continuation twice, once with `true`
and once with `false`. Since the state handler is nested in the
ambiguity handler, it will be reset for the second invocation with `false`
and thus the second element in the resulting list is `0`.

Commuting the two handlers, we get different results:

```scala mdoc:silent
val result2 = state(0) { implicit s: State[Int] =>
  ambList { implicit a: Amb =>
    example
  }
}
```
```scala mdoc
result2.run()
```

Now, ambiguity is run inside of state and the change of state carries
over to the second coin flipping result.

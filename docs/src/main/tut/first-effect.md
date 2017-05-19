---
layout: page
title:  "Your First Effect"
section: "guides"
position: 2
---

# Define your first effect

```tut:book:silent
import effekt._
```

To define our first effect, we need to specify the effect signature.

```tut:book:silent
trait Amb extends Eff { self =>
  def flip[R](): Boolean @@ R
}
object Amb {
  def flip()(implicit u: Use[Amb]) = use(u) { u.effect.flip() }
}
```
There are a few important things about the above definition.

All effectful operations need to be parametric in the answer type `R`.
We will later see why we need `A @@ R`, for now it is enough to know
that it is a shorthand for:

```
State => (A => State => Control[Out[R]]) => Control[Out[R]]
```

The companion object defines a second variant of `flip` that is more
convenient to use. The type `Use[E]` can be read as a capability to
perform all effect operations defined in `E`.

## Using the `Amb` effect

Now let's write a program that uses the ambiguity effect. We flip
a coin once and then return 2 or 3 depending on the result.

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
object ambList extends Amb {
  type Out[A] = List[A]
  type State = Unit // We don't use the state, yet.
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

## Handling Effects
To handle an effect we use the library function `handle(handler)(state)(prog)` which gives
us a capability for the effect type that `handler` implements.

```tut:book:silent
val handled: Control[List[Int]] = handle(ambList)(()) { implicit h => prog }
```

After all effects are handled, we can run the effectful computation:

```tut
handled.run()
```

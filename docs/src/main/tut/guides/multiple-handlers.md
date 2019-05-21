---
layout: docs
title:  "Multiple Handlers at Once"
section: "guides"
---

# Handling Multiple Effects at Once

**THIS EXAMPLE IS OUTDATED! It needs to be updated, once specialized state is supported again**

Sometimes the granularity of handlers does not coincide with the
granularity of effect signatures. For instance, we might want to
offer fine grained interfaces for reading and writing operations
but handle them together in one handler.

In this tutorial, we'll see that in **Effekt** it is very natural to
define such combined handlers. Let's start with defining the
effect signatures for `Reader` and `Writer`.

You can play around with the full source code for this example at this
[Scastie (Scala 2.12)](https://scastie.scala-lang.org/xsU2asSTQNiEI93ocjHpSg).

```tut:book:silent
import effekt._

trait Reader[S] {
  def read(): Control[S]
}
trait Writer[S] {
  def write(s: S): Control[Unit]
}
```

(We omit the standard companion object definitions as seen in the
earlier tutorials.)

## Defining the combined handler

Since handlers of algebraic effects are just implementations of
the effect signature traits it is not surprising that one handler
might implement multiple of such signatures. Below, we define a
simple handler for `Reader` and `Writer` that uses a list to mediate
between the two effects.

```
def rwHandler[R, S] = new Handler[R, List[R]] with Reader[S] with Writer[S] {

  def unit = a => a

  def read() = {
    case s :: rest => resume => resume(s)(rest)
    case _ => sys error "Not enough elements written to perform read"
  }
  def write(s: S) = rest => resume => resume(())(s :: rest)
}
```

## Example usage

To show the usage of the combined handler, let's first define a
simple example program.
```
def example(implicit r: Use[Reader[Int]], w: Use[Writer[Int]]): Control[Int] =
  for {
    _ <- write(2)
    _ <- write(3)
    x <- read()
    _ <- write(x * 2)
    y <- read()
  } yield y
```

Handling the example with our combined handler, we get:

```
rwHandler(Nil) { implicit rw => example }.run()
```


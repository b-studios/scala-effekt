---
layout: page
title:  "Design Decisions"
section: "design"
position: 1
---

# Design Decisions

This is not the first effect library. There are many cool libraries
out there with different target groups and different philosophies.
The key motivation behind **Effekt** is to bring
[Koka-like](https://koka-lang.github.io/koka/doc/kokaspec.html) algebraic
effects with handlers to the Scala language. In consequence many
design decisions are influenced by Koka.

The design decisions behind Effekt are described in a bit more detail
in this [paper](http://files.b-studios.de/effekt.pdf).

## Shallowly Embedded Effects
Other libraries (like [Eff](https://github.com/atnos-org/eff)) use
free monads to represent effectful operations and define interpreters
for the free monad to implement handling of the effect.

In **Effekt** effect signatures are shallowly embedded. That is,
instead of creating an instance of a `Flip()` class to represent the
action of flipping a coin and later interpreting it, in **Effekt** the
flip operation is immediately called on the corresponding handler:

```
def prog(implicit amb: Use[Amb]): Control[Int] =
  amb.flip() map { x => if (x) 2 else 3 }
```
(**ATTENTION**: The above code is a slight simplification, only for
illustrative purposes. For real code see [Your First Effect](./guides/getting-started.html))

As can be seen above, **Effekt** uses implicit arguments to pass down
handler implementations to the use-site (`flip`).

This works even more nicely in [Dotty](http://dotty.epfl.ch/), where
implicit function types are available:

```
type using[A, E <: Eff] = implicit Use[E] => Control[A]
def prog: Int using Amb = flip() map { x => if (x) 2 else 3 }
```

Pretty neat, isn't it?

We prepared [this Scastie](https://scastie.scala-lang.org/eIMWNq7aRty2YHRuFNQ2BQ) for
you to play around with dotty and **effekt**.

## Delimited Control
As in the Koka language, effect handlers in **Effekt** get access to
the operation's continuation delimited by the handler itself:

```
object ambList extends Amb {
  def flip[R]() = _ => resume => resume(true)
  ...
}
```
The continuation may be called zero to arbitrary many times. To
implement this functionality, **Effekt** is based on a specialized
variant of the `CC`-monad as introduced in

> **A Monadic Framework for Delimited Continuations**
> by R. Kent Dybvig, Simon Peyton Jones and Amr Sabry (2007), [PDF](https://www.cs.indiana.edu/~sabry/papers/monadicDC.pdf)

In **Effekt** the control-monad is called `Control`, no surprise there.

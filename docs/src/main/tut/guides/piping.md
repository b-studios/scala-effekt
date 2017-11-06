---
layout: docs
title:  "Pipes: Connecting Producers and Consumers"
section: "guides"
---

# Pipes: Connecting Producers and Consumers
In this quick guide, we'll re-implement the piping example from the
paper ["Handlers in Action"](http://homepages.inf.ed.ac.uk/slindley/papers/handlers.pdf)
by Ohad Kammar and colleagues.
In particular, we are interested in treating sending and receiving of
information as effects.
For simplicity, we'll restrict ourselves to the case where the sent
information is of type `Int`.

First of all, let's define the effect signatures for sending and
receiving of information:

```tut:book:silent
import effekt._

trait Send extends Eff {
  def send(n: Int): Op[Unit]
}
trait Receive extends Eff {
  def receive(): Op[Int]
}
```
```tut:book:silent:invisible
object Send {
  def send(n: Int)(implicit u: Use[Send]) = use(u) { u.handler.send(n) }
}

object Receive {
  def receive()(implicit u: Use[Receive]) = use(u) { u.handler.receive() }
}
import Send._, Receive._
```
Now, using these effect signatures we can define an example producer and
a corresponding example consumer:

```tut:book:silent
def producer(implicit s: Use[Send]): Control[Unit] =
  for {
    _ <- send(1)
    _ <- send(2)
    _ <- send(3)
  } yield ()

def consumer(implicit s: Use[Receive]): Control[Unit] =
  for {
    x1 <- receive()
    _ = println("1: " + x1)
    x2 <- receive()
    _ = println("2: " + x2)
    x3 <- receive()
    _ = println("3: " + x3)
  } yield ()
```

What is missing, is a way to connect the two in order to form a pipe.
The core insight to achieve this the following:
The two ends of a pipe are connected by holding a reference to the
opposite end as part of their internal state.
After performing an
action, such as sending or receiving data, the current process is
paused and the state of the opposite process is updated with the
current continuation.

Now let's implement this behavior in Effekt. To this end, we define
processes as handlers and use the state of the handler to store the
opposite end:

```tut:book:silent
trait Process[R0, P[_]] extends Handler {
  type R     = R0
  type Res   = R
  type State = P[Control[R]]
  def unit = identity
}
```
In our case, the type constructor `P[_]` will be one of the following:

```tut:book:silent
object Process {
  case class Prod[R](apply: Unit => Cons[R] => R)
  case class Cons[R](apply: Int  => Prod[R] => R)
}
import Process._
```
Note that each process holds a **continuation** which takes the resulting
value as first argument **and the updated opposite process** as state
(second argument) to finally compute an `R`. As can be seen in the
definition of `Process` above, the `R` will eventually be instantiated
as `Control[R0]` since it needs to be effectful.

The two handlers corresponding to receiving and producing processes
now can be defined as:

```tut:book:silent
def down[R] = new Receive with Process[R, Prod] {
  def receive() = {
    case Prod(prod) => resume => prod(())(Cons(resume))
  }
}

def up[R] = new Send with Process[R, Cons] {
  def send(n: Int) = {
    case Cons(cons) => resume => cons(n)(Prod(resume))
  }
}
```
Stunning symmetry, isn't it? :)

Finally, the pipe can be created by connecting `up` and `down` to
handle two program fragments using `Receive` and `Send` correspondingly.

```tut:book:silent
def pipe[R](d: Use[Receive] => Control[R], u: Use[Send] => Control[R]): Control[R] =
  down[R](Prod(_ => cons => up(cons) { u })) { d }
```
Running our above example with `pipe` yields:
```tut
pipe(d => consumer(d), u => producer(u)).run()
```

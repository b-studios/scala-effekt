---
layout: home
---

# DISCONTINUED

The Scala library "Scala Effekt" is discontinued. In 2020, its development has been superseded by our own [_standalone programming language_ **Effekt**](https://effekt-lang.org/), independent of Scala.

Due to its origins, but confusingly, both carry **Effekt** in their name. To disambiguate, we recommend to use **Scala Effekt** to refer to the library and **Effekt** to the standalone programming language.

The evolution of the Effekt language and its predecessors is [explained here](https://effekt-lang.org/evolution).

# Extensible, effectful DSLs
Create extensible, effectful domain specific
languages while separating the effect definition from the effect
implementation.
**Scala Effekt** is an implementation of *algebraic effects
with handlers* and allows you to structure your effectful programs in a
purely functional way. It thus represents an alternative to
monad transformer based program structuring techniques or free monads.

<section class="home-box" markdown="1">
<div class="container" markdown="1">
<div class="row" markdown="1">
<div class="col-md-5" markdown="1">
### An example
The `Twitter`-API example shows how to define an effect signature with
one effectful operation `userTweets`. The effect itself can be
implemented in many ways by simply implementing the abstract methods
in the `Twitter` trait. It could actually contact the Twitter API
(potentially using other effects, like HTTP in the implementation) or
it could just return dummy tweets for testing purposes. We chose the
latter for the implementation of `twitterStub` and thus running the
result actually perform the side-effects always gives the same results.

You can find the full sources for this example in [this Dotty Scastie](https://scastie.scala-lang.org/JplohyA1RWeE7ykU08BqwQ).
</div>
<div class="col-md-7" markdown="1">
```scala mdoc:invisible
import effekt._
case class Tweet(msg: String)
```
**Effect signature**
```scala mdoc:silent
trait Twitter {
  def userTweets(userId: Long): Control[List[Tweet]]
}
```
```scala mdoc:invisible
class TwitterStub[R] extends Twitter with Handler[R] {
  def userTweets(userId: Long): Control[List[Tweet]] = pure(List(Tweet("hi")))
}
def twitterStub[R] = new TwitterStub[R]
```
**Effect usage**
```scala mdoc:silent
def query(T: Twitter): Control[List[Tweet]] =
  for {
    ts1 <- T.userTweets(133452)
    ts2 <- T.userTweets(111345)
  } yield ts1 ++ ts2

val result = twitterStub { query }
```
</div>
</div>
</div>
</section>

## Getting Started
To use **Effekt** (tested with Scala 2.12 and Scala 2.13), include the
following line to your `build.sbt` file:

```
resolvers += Opts.resolver.sonatypeSnapshots

libraryDependencies += "de.b-studios" %% "effekt" % "0.4-SNAPSHOT"
```

To learn how to use the library, see [Your First Effect](./guides/getting-started.html).

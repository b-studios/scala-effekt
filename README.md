[![Chat](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/scala-effekt/scala-effekt)

# DISCONTINUED

The Scala library "Scala Effekt" is discontinued. In 2020, its development has been superseded by our own [_standalone programming language_ **Effekt**](https://effekt-lang.org/), independent of Scala.

Due to its origins, but confusingly, both carry **Effekt** in their name. To disambiguate, we recommend to use **Scala Effekt** to refer to the library and **Effekt** to the standalone programming language.

The evolution of the Effekt language and its predecessors is [explained here](https://effekt-lang.org/evolution).

# Scala Effekt
The **Effekt** library allows you to structure your effectful programs
in a functional way. It thus represents an alternative to traditional
monad transformer based program structuring techniques.

To use **Effekt** (tested with Scala 2.12 and Scala 2.13), include the
following line to your `build.sbt` file:

```scala
resolvers += Opts.resolver.sonatypeSnapshots

libraryDependencies += "de.b-studios" %% "effekt" % "0.4-SNAPSHOT"
```

To learn how to use the library, see [Your First Effect](http://b-studios.de/scala-effekt/guides/getting-started.html).

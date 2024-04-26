[![Chat](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/scala-effekt/scala-effekt)

# Disclaimer
This Scala library **is different from** the equally named standalone ["Effekt language"](http://effekt-lang.org), developed by the same authors. Recent publications only address the latter.

# Scala Effekt
The **Effekt** library allows you to structure your effectful programs
in a functional way. It thus represents an alternative to traditional
monad transformer based program structuring techniques.

To use **Effekt** (tested with Scala 2.12 and Scala 2.13, and Scala 3 is supported), include the
following line to your `build.sbt` file:

```scala
resolvers += Opts.resolver.sonatypeSnapshots

libraryDependencies += "de.b-studios" %% "effekt" % "0.4-SNAPSHOT"
```

To learn how to use the library, see [Your First Effect](http://b-studios.de/scala-effekt/guides/getting-started.html).

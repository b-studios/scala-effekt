# Scala Effekt
The **Effekt** library allows you to structure your effectful programs
in a functional way. It thus represents an alternative to traditional
monad transformer based program structuring techniques.

The verison of **Effekt** in this branch (which requires Dotty) is not yet published. 
So you need to clone the repo and run `sbt` yourselves.

The API is mostly the one as described in [this paper](http://ps.informatik.uni-tuebingen.de/publications/brachthaeuser19effekt/).
However, there are two versions.

#### Unsafe API
The unsafe API does not guarantee effect safety (hence the name). Not having to care for effect safety 
makes writing (and applying) some handlers easier.

#### Safe API
The effect safe version is almost exactly the one presented in [the paper](http://ps.informatik.uni-tuebingen.de/publications/brachthaeuser19effekt/). The operational semantics of both APIs is the same and
in this branch we actually implement the unsafe API by forgetting the effect types.

Make sure to check out the examples. Most examples exist for both, the effect safe and unsafe API.

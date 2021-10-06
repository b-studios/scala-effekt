// When the user clicks on the search box, we want to toggle the search dropdown
function displayToggleSearch(e) {
  e.preventDefault();
  e.stopPropagation();

  closeDropdownSearch(e);
  
  if (idx === null) {
    console.log("Building search index...");
    prepareIdxAndDocMap();
    console.log("Search index built.");
  }
  const dropdown = document.querySelector("#search-dropdown-content");
  if (dropdown) {
    if (!dropdown.classList.contains("show")) {
      dropdown.classList.add("show");
    }
    document.addEventListener("click", closeDropdownSearch);
    document.addEventListener("keydown", searchOnKeyDown);
    document.addEventListener("keyup", searchOnKeyUp);
  }
}

//We want to prepare the index only after clicking the search bar
var idx = null
const docMap = new Map()

function prepareIdxAndDocMap() {
  const docs = [  
    {
      "title": "ANF Transformation using Algebraic Effects",
      "url": "/scala-effekt/guides/a-normal-form.html",
      "content": "ANF Transformation using Algebraic Effects ANF (or A-normal-form) is a program representation very commonly used in compilers. The basic idea is, that all non-trivial terms (in a very wide sense, those that correspond to computation, or invoke side-effects) are bound to variables. For example, translating 1 - ((3 - 2) - 4) to ANF gives var x0 = 3 - 2 var x1 = x0 - 4 var x2 = 1 - x1 x2 ANF transformations can be implemented using continuation passing style, as demonstrated by Matt Might. In this short guide, we will see how algebraic effects can be used to describe an ANF transformation for a very simple expression language. trait Exp case class Lit(n: Int) extends Exp case class Sub(l: Exp, r: Exp) extends Exp case class Var(name: String) extends Exp case class Let(name: String, value: Exp, body: Exp) extends Exp The language features numeric literals, subtraction, let-bindings and variables. The above example in our small language reads as: val ex = Sub(Lit(1), Sub(Sub(Lit(3), Lit(2)), Lit(4))) To implement the transformation we define a Transform effect that we use to mark elements of our language as either values or computations: import effekt._ trait Transform { def value(e: Exp): Control[Exp] def computation(e: Exp): Control[Exp] } We also define a recursive traversal function that uses Transform to visit each node in our tree: def visit(e: Exp)(implicit t: Transform): Control[Exp] = e match { case v : Var =&gt; t.value(v) case l : Lit =&gt; t.value(l) case Let(name, v, b) =&gt; for { vv &lt;- visit(v) bv &lt;- visit(b) e &lt;- t.value(Let(name, vv, bv)) } yield e case Sub(l, r) =&gt; for { lv &lt;- visit(l) rv &lt;- visit(r) e &lt;- t.computation(Sub(lv, rv)) } yield e } This traversal marks variables and literals as values, traverses the subcomponents of Let and Sub and finally marks let bindings as values and subtraction as computation. Intermezzo: Generating fresh names To define our ANF transformation we need to come up with fresh names such as x1, x2 above. Generating fresh variable names / symbols can be seen as an effect, so we define another effect signature for SymGen: trait SymGen { def fresh(): Control[String] } A handler for fresh can be implemented using stateful-handlers, as provided by effekt: class SymState[R] extends SymGen with Handler[R] with State { val count = Field(0) private def inc = for { x &lt;- count.value _ &lt;- count.value = x + 1 } yield x def fresh() = inc map { \"x\" + _ } } Defining an ANF handler for Transform With the ability to generate fresh names, we are now ready to define an ANF transformation as a handler for Transform: class ANF(implicit sym: SymGen) extends Transform with Handler[Exp] { def value(e: Exp) = use { resume =&gt; resume(e) } def computation(e: Exp) = use { resume =&gt; for { x &lt;- sym.fresh() body &lt;- resume(Var(x)) } yield Let(x, e, body) } } def ANF(prog: Transform =&gt; Control[Exp])(implicit sym: SymGen): Control[Exp] = (new ANF).apply(prog) Note that this handler itself is effectful and uses the SymGen effect. To do so, it requires a capability as constructor argument. We only need the Basic handler here and discard the handler state (hence the _). For values, we just resume with the expression unchanged. However, for computations we generate a fresh name, build the corresponding tree using the freshly generated variable and wrap it finally in a let-binding. It is important to note that this let-binding will be introduced at the point where the ANF handler is used. Finally the anf-transformation can be defined in terms of the SymState handler and the ANF handler: def anfTransform(e: Exp): Control[Exp] = new SymState handle { implicit sym: SymGen =&gt; new ANF handle { implicit anf: Transform =&gt; visit(e) } } Calling anfTransform on our running example, we obtain: run { anfTransform(ex) } // res2: Exp = Let( // \"x0\", // Sub(Lit(3), Lit(2)), // Let( // \"x1\", // Sub(Var(\"x0\"), Lit(4)), // Let(\"x2\", Sub(Lit(1), Var(\"x1\")), Var(\"x2\")) // ) // ) which exactly corresponds to our manual translation from above."
    } ,    
    {
      "title": "Design Decisions",
      "url": "/scala-effekt/design.html",
      "content": "Design Decisions This is not the first effect library. There are many cool libraries out there with different target groups and different philosophies. The key motivation behind Effekt is to bring Koka-like algebraic effects with handlers to the Scala language. In consequence many design decisions are influenced by Koka. The design decisions behind Effekt are described in a bit more detail in this paper. Shallowly Embedded Effects Other libraries (like Eff) use free monads to represent effectful operations and define interpreters for the free monad to implement handling of the effect. In Effekt effect signatures are shallowly embedded. That is, instead of creating an instance of a Flip() class to represent the action of flipping a coin and later interpreting it, in Effekt the flip operation is immediately called on the corresponding handler: def prog(implicit amb: Use[Amb]): Control[Int] = amb.flip() map { x =&gt; if (x) 2 else 3 } (ATTENTION: The above code is a slight simplification, only for illustrative purposes. For real code see Your First Effect) As can be seen above, Effekt uses implicit arguments to pass down handler implementations to the use-site (flip). This works even more nicely in Dotty, where implicit function types are available: type using[A, E &lt;: Eff] = implicit Use[E] =&gt; Control[A] def prog: Int using Amb = Amb.flip() map { x =&gt; if (x) 2 else 3 } Pretty neat, isn’t it? We prepared this Scastie for you to play around with dotty and effekt. Delimited Control As in the Koka language, effect handlers in Effekt get access to the operation’s continuation delimited by the handler itself: object ambList extends Amb { def flip[R]() = use { resume =&gt; resume(true) } } The continuation may be called zero to arbitrary many times. To implement this functionality, Effekt is based on a specialized variant of the CC-monad as introduced in A Monadic Framework for Delimited Continuations by R. Kent Dybvig, Simon Peyton Jones and Amr Sabry (2007), PDF In Effekt the control-monad is called Control, no surprise there."
    } ,    
    {
      "title": "Getting Started",
      "url": "/scala-effekt/guides/getting-started.html",
      "content": "Getting Started Before you can get started with Effekt you need to set up your environment and add Effekt as a dependency to your build.sbt file: resolvers += Opts.resolver.sonatypeSnapshots libraryDependencies += \"de.b-studios\" %% \"effekt\" % \"0.4-SNAPSHOT\" Alternatively, to play around with Effekt in your browser, you can also use Scastie. We prepared two different Scasties for you Scala 2.12 Dotty – Here you can also play around with implicit function types. The interface is slightly different at the moment. Define your first effect First we import all the necessary types and functions. Then, to define our first effect, we specify the effect signature. import effekt._ // The effect signature trait Amb { def flip(): Control[Boolean] } Effect signatures in Effekt are ordinary Scala traits that simply declare the effect operations. For programming with effects the most important type is Control. For now we only have to know that all effect operations like flip have to be marked as returning something in Control. Using the Amb effect To actually use the flip effect we need to get our hands on a capability (that is, an instance of the effect signature Amb) that entitles us to do so. Since we don’t yet know where to get such capabilities from, we just define a function asking for it. We flip a coin once and then return 2 or 3 depending on the result. def prog(amb: Amb): Control[Int] = for { x &lt;- amb.flip() } yield if (x) 2 else 3 The result type Control[Int] tells us that the integer-result will be contained in the Control monad which is Effekt specific. Defining Handlers Let us now define our own handler for the Amb effect. A handler is just an implementation of the effect interface. However, it also needs to give the type to interpret the effect into (List[R]). Note how we make sure that the result of the handled program is a list by mapping r =&gt; List(r) over it before applying the handler. This way we convert the result type from R into the semantic domain List[R]. def ambList[R](prog: Amb =&gt; Control[R]): Control[List[R]] = new Handler[List[R]] with Amb { def flip() = use { resume =&gt; for { ts &lt;- resume(true) fs &lt;- resume(false) } yield ts ++ fs } } handle { amb =&gt; prog(amb) map { r =&gt; List(r) } } Notice how we use the trait Handler which provides us with the method use. To implement flip we get access to the continuation resume (the remaining program after the flip effect, up to the handler). The type of the expression passed to use is thus: type Body = (Boolean =&gt; Control[List[R]]) =&gt; Control[List[R] // ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ // the continuation Note how both, the continuation and the result type, coincide to be Control[List[R]]. Handling Effects To handle an effect we use the apply method that is defined on an instance of handler. val handled: Control[List[Int]] = ambList { amb =&gt; prog(amb) } One way of thinking about algebraic effects and handlers is to think as (resumable) exceptions. In our example, the effect operation flip would correspond to throw and the handler ambList to a surrounding try { ... }. After all effects are handled, we can now run the effectful computation: run { handled } // res0: List[Int] = List(2, 3)"
    } ,    
    {
      "title": "Guides & Examples",
      "url": "/scala-effekt/guides.html",
      "content": "Guides &amp; Examples ANF Transformation using Algebraic Effects Getting Started Multiple Effects Multiple Handlers at Once Pipes: Connecting Producers and Consumers"
    } ,        
    {
      "title": "Multiple Effects",
      "url": "/scala-effekt/guides/multiple-effects.html",
      "content": "Combining Multiple Effects with Handlers After having defined a single effect in Getting Started, this quick tutorial shows how multiple, different effects can be defined and handled in Effekt. Again, we prepared Scasties to follow along with this tutorial. Scala 2.12 (full solution) Defining a second effect In Getting Started we have defined ambiguity as our first effect. The ambiguity effect signature had one effect operation and looked like this: import effekt._ trait Amb { def flip(): Control[Boolean] } def ambList[R](prog: Amb =&gt; Control[R]): Control[List[R]] = new Handler[List[R]] with Amb { def flip() = use { resume =&gt; for { ts &lt;- resume(true) fs &lt;- resume(false) } yield ts ++ fs } } handle { amb =&gt; prog(amb) map { r =&gt; List(r) } } We also defined a handler for the ambiguity effect as an implementation of the Amb trait, called ambList. To see how to combine two different effects, we will now first define a second (quite standard) effect: Mutable state. As before we start with the effect signature. trait State[S] { def get(): Control[S] def put(s: S): Control[Unit] } Having defined the effect signature, we can implement a handler that, like the state monad, does not actually use mutable state but passes the current value around through the whole program. def state[R, S](init: S)(prog: State[S] =&gt; Control[R]): Control[R] = new Handler[S =&gt; Control[R]] with State[S] { def put(s: S) = use { resume =&gt; pure { s2 =&gt; resume(()) flatMap { _ apply s } } } def get() = use { resume =&gt; pure { s2 =&gt; resume(s2) flatMap { _ apply s2 } } } } handle { state =&gt; prog(state) map { r =&gt; s =&gt; pure(r) } } flatMap { _ apply init } Using Amb and State in one example Let us now use the two effects to write a program that combines them. def example(implicit s: State[Int], amb: Amb): Control[Int] = for { x &lt;- s.get() b &lt;- amb.flip() _ &lt;- if (b) s.put(x + 1) else pure(()) y &lt;- s.get() } yield (x + y) The example program requires capabilities for both effects, a state effect carrying an integer and the ambiguity effect. We first retreive the current state, then toss a coin and depending on result mutate the state (or not). As you can see, there is nothing special to using more than one effect. The use of effects naturally composes. Handling the two effects As with monad transformers, we have two different ways of handling the two effects. Should we first handle away the ambiguity effect and then consider state, or the other way around? The nice thing with algebraic effects and handlers is that we can decide very late. Let’s experiment with the two options: val result1: Control[List[Int]] = ambList { implicit a: Amb =&gt; state(0) { implicit s: State[Int] =&gt; example } } result1.run() // res0: List[Int] = List(1, 0) In this variant, we first handle away the state effect and then handle ambiguity. The ambiguity handler invokes the continuation twice, once with true and once with false. Since the state handler is nested in the ambiguity handler, it will be reset for the second invocation with false and thus the second element in the resulting list is 0. Commuting the two handlers, we get different results: val result2 = state(0) { implicit s: State[Int] =&gt; ambList { implicit a: Amb =&gt; example } } result2.run() // res1: List[Int] = List(1, 1) Now, ambiguity is run inside of state and the change of state carries over to the second coin flipping result."
    } ,    
    {
      "title": "Multiple Handlers at Once",
      "url": "/scala-effekt/guides/multiple-handlers.html",
      "content": "Handling Multiple Effects at Once Sometimes the granularity of handlers does not coincide with the granularity of effect signatures. For instance, we might want to offer fine grained interfaces for reading and writing operations but handle them together in one handler. In this tutorial, we’ll see that in Effekt it is very natural to define such combined handlers. Let’s start with defining the effect signatures for Reader and Writer. You can play around with the full source code for this example at this Scastie (Scala 2.12). import effekt._ trait Reader[S] { def read(): Control[S] } trait Writer[S] { def write(s: S): Control[Unit] } (We omit the standard companion object definitions as seen in the earlier tutorials.) Defining the combined handler Since handlers of algebraic effects are just implementations of the effect signature traits it is not surprising that one handler might implement multiple of such signatures. Below, we define a simple handler for Reader and Writer that uses a list to mediate between the two effects. def rwHandler[R, S] = new Handler[R] with Reader[S] with Writer[S] with State { // create a new field val out = Field(List.empty[S]) def read() = out.value flatMap { case s :: rest =&gt; for { _ &lt;- out.value = rest } yield s case _ =&gt; sys error \"Not enough elements written to perform read\" } def write(s: S) = for { rest &lt;- out.value _ &lt;- out.value = s :: rest } yield () } Example usage To show the usage of the combined handler, let’s first define a simple example program. def example(implicit r: Reader[Int], w: Writer[Int]): Control[Int] = for { _ &lt;- w.write(2) _ &lt;- w.write(3) x &lt;- r.read() _ &lt;- w.write(x * 2) y &lt;- r.read() } yield y Handling the example with our combined handler, we get: run { rwHandler handle { implicit rw: Reader[Int] with Writer[Int] =&gt; example } } // res0: Int = 6"
    } ,    
    {
      "title": "Pipes: Connecting Producers and Consumers",
      "url": "/scala-effekt/guides/piping.html",
      "content": "Pipes: Connecting Producers and Consumers In this quick guide, we’ll re-implement the piping example from the paper “Handlers in Action” by Ohad Kammar and colleagues. In particular, we are interested in treating sending and receiving of information as effects. For simplicity, we’ll restrict ourselves to the case where the sent information is of type Int. First of all, let’s define the effect signatures for sending and receiving of information: import effekt._ trait Send { def send(n: Int): Control[Unit] } trait Receive { def receive(): Control[Int] } Now, using these effect signatures we can define an example producer and a corresponding example consumer: def producer(s: Send): Control[Unit] = for { _ &lt;- s.send(1) _ &lt;- s.send(2) _ &lt;- s.send(3) } yield () def consumer(r: Receive): Control[Unit] = for { x1 &lt;- r.receive() _ = println(\"1: \" + x1) x2 &lt;- r.receive() _ = println(\"2: \" + x2) x3 &lt;- r.receive() _ = println(\"3: \" + x3) } yield () What is missing, is a way to connect the two in order to form a pipe. The core insight to achieve this the following: The two ends of a pipe are connected by holding a reference to the opposite end as part of their internal state. After performing an action, such as sending or receiving data, the current process is paused and the state of the opposite process is updated with the current continuation. Now let’s implement this behavior in Effekt. To this end, we define processes as handlers and use the state of the handler to store the opposite end: class Process[R, P[_]](val init: P[Control[R]]) extends Handler.Stateful[R, P[Control[R]]] In our case, the type constructor P[_] will be one of the following: object Process { case class Prod[R](apply: Unit =&gt; Cons[R] =&gt; R) case class Cons[R](apply: Int =&gt; Prod[R] =&gt; R) } import Process._ Note that each process holds a continuation which takes the resulting value as first argument and the updated opposite process as state (second argument) to finally compute an R. As can be seen in the definition of Process above, the R will eventually be instantiated as Control[R0] since it needs to be effectful. The two handlers corresponding to receiving and producing processes now can be defined as: def down[R](p: Prod[Control[R]]) = new Process[R, Prod](p) with Receive { def receive() = useState { case Prod(prod) =&gt; resume =&gt; prod(())(Cons(resume)) } } def up[R](p: Cons[Control[R]]) = new Process[R, Cons](p) with Send { def send(n: Int) = useState { case Cons(cons) =&gt; resume =&gt; cons(n)(Prod(resume)) } } Stunning symmetry, isn’t it? :) Finally, the pipe can be created by connecting up and down to handle two program fragments using Receive and Send correspondingly. def pipe[R](d: Receive =&gt; Control[R], u: Send =&gt; Control[R]): Control[R] = down[R](Prod(_ =&gt; cons =&gt; up(cons) { u })) { d } Running our above example with pipe yields: pipe(d =&gt; consumer(d), u =&gt; producer(u)).run() // 1: 1 // 2: 2 // 3: 3"
    } ,      
  ];

  idx = lunr(function () {
    this.ref("title");
    this.field("content");

    docs.forEach(function (doc) {
      this.add(doc);
    }, this);
  });

  docs.forEach(function (doc) {
    docMap.set(doc.title, doc.url);
  });
}

// The onkeypress handler for search functionality
function searchOnKeyDown(e) {
  const keyCode = e.keyCode;
  const parent = e.target.parentElement;
  const isSearchBar = e.target.id === "search-bar";
  const isSearchResult = parent ? parent.id.startsWith("result-") : false;
  const isSearchBarOrResult = isSearchBar || isSearchResult;

  if (keyCode === 40 && isSearchBarOrResult) {
    // On 'down', try to navigate down the search results
    e.preventDefault();
    e.stopPropagation();
    selectDown(e);
  } else if (keyCode === 38 && isSearchBarOrResult) {
    // On 'up', try to navigate up the search results
    e.preventDefault();
    e.stopPropagation();
    selectUp(e);
  } else if (keyCode === 27 && isSearchBarOrResult) {
    // On 'ESC', close the search dropdown
    e.preventDefault();
    e.stopPropagation();
    closeDropdownSearch(e);
  }
}

// Search is only done on key-up so that the search terms are properly propagated
function searchOnKeyUp(e) {
  // Filter out up, down, esc keys
  const keyCode = e.keyCode;
  const cannotBe = [40, 38, 27];
  const isSearchBar = e.target.id === "search-bar";
  const keyIsNotWrong = !cannotBe.includes(keyCode);
  if (isSearchBar && keyIsNotWrong) {
    // Try to run a search
    runSearch(e);
  }
}

// Move the cursor up the search list
function selectUp(e) {
  if (e.target.parentElement.id.startsWith("result-")) {
    const index = parseInt(e.target.parentElement.id.substring(7));
    if (!isNaN(index) && (index > 0)) {
      const nextIndexStr = "result-" + (index - 1);
      const querySel = "li[id$='" + nextIndexStr + "'";
      const nextResult = document.querySelector(querySel);
      if (nextResult) {
        nextResult.firstChild.focus();
      }
    }
  }
}

// Move the cursor down the search list
function selectDown(e) {
  if (e.target.id === "search-bar") {
    const firstResult = document.querySelector("li[id$='result-0']");
    if (firstResult) {
      firstResult.firstChild.focus();
    }
  } else if (e.target.parentElement.id.startsWith("result-")) {
    const index = parseInt(e.target.parentElement.id.substring(7));
    if (!isNaN(index)) {
      const nextIndexStr = "result-" + (index + 1);
      const querySel = "li[id$='" + nextIndexStr + "'";
      const nextResult = document.querySelector(querySel);
      if (nextResult) {
        nextResult.firstChild.focus();
      }
    }
  }
}

// Search for whatever the user has typed so far
function runSearch(e) {
  if (e.target.value === "") {
    // On empty string, remove all search results
    // Otherwise this may show all results as everything is a "match"
    applySearchResults([]);
  } else {
    const tokens = e.target.value.split(" ");
    const moddedTokens = tokens.map(function (token) {
      // "*" + token + "*"
      return token;
    })
    const searchTerm = moddedTokens.join(" ");
    const searchResults = idx.search(searchTerm);
    const mapResults = searchResults.map(function (result) {
      const resultUrl = docMap.get(result.ref);
      return { name: result.ref, url: resultUrl };
    })

    applySearchResults(mapResults);
  }

}

// After a search, modify the search dropdown to contain the search results
function applySearchResults(results) {
  const dropdown = document.querySelector("div[id$='search-dropdown'] > .dropdown-content.show");
  if (dropdown) {
    //Remove each child
    while (dropdown.firstChild) {
      dropdown.removeChild(dropdown.firstChild);
    }

    //Add each result as an element in the list
    results.forEach(function (result, i) {
      const elem = document.createElement("li");
      elem.setAttribute("class", "dropdown-item");
      elem.setAttribute("id", "result-" + i);

      const elemLink = document.createElement("a");
      elemLink.setAttribute("title", result.name);
      elemLink.setAttribute("href", result.url);
      elemLink.setAttribute("class", "dropdown-item-link");

      const elemLinkText = document.createElement("span");
      elemLinkText.setAttribute("class", "dropdown-item-link-text");
      elemLinkText.innerHTML = result.name;

      elemLink.appendChild(elemLinkText);
      elem.appendChild(elemLink);
      dropdown.appendChild(elem);
    });
  }
}

// Close the dropdown if the user clicks (only) outside of it
function closeDropdownSearch(e) {
  // Check if where we're clicking is the search dropdown
  if (e.target.id !== "search-bar") {
    const dropdown = document.querySelector("div[id$='search-dropdown'] > .dropdown-content.show");
    if (dropdown) {
      dropdown.classList.remove("show");
      document.documentElement.removeEventListener("click", closeDropdownSearch);
    }
  }
}

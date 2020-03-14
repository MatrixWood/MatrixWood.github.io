---
layout: post
title: "Uniting Church and State: FP and OO Together"
author: Noel Welsh
---

This is a post about church and state, and how we can unite the two for a better world, while avoiding unfortunate side effects.

Political metaphors aside, this really *is* a post about Church---[Alonzo Church](alonzo-church)---and how we can use his idea of Church encoding to unite pure FP and imperative OO to achieve, if not a better world, at least better code. 

<!--more-->
  
But, you say, Scala already claims to be an object-functional hybrid, so why do we need to know about Church encodings?

I'm glad you asked.
  
Although Scala provides both OO and FP language features this is not sufficient to give coherent design principles encompassing both paradigms. Scala usage tends to fracture into FP or OO camps (I'm in the FP camp) as multi-paradigm design is hard.
  
The Church encoding provides such a unifying design principle.

In what follows I'm going to show that

  - FP and OO code make different tradeoffs with respect to extensibility, static guarantees, and performance.
  - Church encoding gives us a way to transform FP designs into OO designs.
  - We can use the inverse of Church encoding, called reification, to transform OO designs to FP.
  - This means we can keep one coherent design or mental model and implement it in FP or OO style according to the tradeoffs we want to make, meaning we can truly unify FP and OO.
  - This is useful, as seen in a real commercial system Underscore developed that uses Church encoding for a big performance improvement.
  - Finally, the idea of Church encoding gives us a way to unify other FP techniques that are creating buzz in the Scala community: Free structures, and tagless final interpreters.

## FP vs OO

We are going to start by reviewing classic FP and OO techniques, and see how the two techniques make different tradeoffs in terms of extensibility.

Our example will be a very simple calculator. We'll just have basic arithemetic operations: add, subtract, multiply and divide.


### Classic OO

We can argue about what makes good OO design, but here's some code that I think is reasonably typical and very straightforward:

```scala
class Calculator {
  def literal(v: Double): Double = v
  def add(a: Double, b: Double): Double = a + b
  def subtract(a: Double, b: Double): Double = a - b
  def multiply(a: Double, b: Double): Double = a * b
  def divide(a: Double, b: Double): Double = a / b
}
```

In this design we can easily add new calculation operations, by creating a subclass. For example, if we want to add trigonometric functions like `cos`, `sin`, we can create a subclass `TrigCalculator` and add them there.

```scala
class TrigCalculator extends Calculator {
  def sin(v: Double): Double = Math.sin(v)
  def cos(v: Double): Double = Math.cos(v)
}
```

We can't easily change the way we perform these operations. If we want to pretty-print expressions, returning a `String`, or compute with `BigDecimal` for exact results, we have to change all our existing code.
     
In summary it is easy to add new operations to our calculator (methods) but hard to add new actions (result types).

### Classic FP

Now let's see how we might approach the same problem from a classic FP perspective. Again, we're going for a very basic approach like we might write in, say, ML.

The first step is to implement an algebraic data type describing the computations we allow.

```scala
sealed trait Calculation
final case class Literal(v: Double) extends Calculation
final case class Add(a: Calculation, b: Calculation) extends Calculation
final case class Subtract(a: Calculation, b: Calculation) extends Calculation
final case class Multiply(a: Calculation, b: Calculation) extends Calculation
final case class Divide(a: Calculation, b: Calculation) extends Calculation
```
   
Now we implement a method to evaluate these expressions:

```scala
def eval(c: Calculation): Double = 
  c match {
    case Literal(v)     => v
    case Add(a, b)      => eval(a) + eval(b)
    case Subtract(a, b) => eval(a) - eval(b)
    case Multiply(a, b) => eval(a) * eval(b)
    case Divide(a, b)   => eval(a) / eval(b)
  }
```
   
   
With this design it's easy to add new actions. 
If we want to pretty print, for example, we can easily implement that as a new function with the same structure as `eval`. 


```scala
def prettyPrint(c: Calculation): String = 
  c match {
    case Literal(v)     => v
    case Add(a, b)      => s"${prettyPrint(a)} + ${prettyPrint(b)}"
    case Subtract(a, b) => s"${prettyPrint(a)} - ${prettyPrint(b)}"
    case Multiply(a, b) => s"${prettyPrint(a)} * ${prettyPrint(b)}"
    case Divide(a, b)   => s"${prettyPrint(a)} / ${prettyPrint(b)}"
  }
```

However it's impossible to add new operations, like `sin` and `cos`, to this representation without code changes.


### Conclusions

We've divided our calculator into two parts:

 - operations, which are the things we want to do (add, subtract, divide, etc.); and
 - actions, which are how we want to do them (using `Double`, pretty print, etc.)

This corresponds to the FP mantra of separating the description of what we want to do (operations) from how we do it (actions).
Notably this is how we control effects in FP---they only happen when executing actions so while we're describing that program we can ignore them.
   
We see that OO and FP allow easy extension in different directions:

 - OO makes it easy to add new operations, but makes adding new actions hard; whereas
 - FP makes it easy to add new actions, but makes adding new operations hard.

## Church Encoding

The Church encoding gives us a way to relate the OO and FP representation.

Let's look at `eval` from the FP implementation

```scala
def eval(c: Calculation): Double = 
  c match {
    case Literal(v)     => v
    case Add(a, b)      => a + b
    case Subtract(a, b) => a - b
    case Multiply(a, b) => a * b
    case Divide(a, b)   => a / b
  }
```
   
and compare it to the OO implementation

```scala
class Calculator {
  def literal(v: Double): Double = v
  def add(a: Double, b: Double): Double = a + b
  def subtract(a: Double, b: Double): Double = a - b
  def multiply(a: Double, b: Double): Double = a * b
  def divide(a: Double, b: Double): Double = a / b
}
```

We can see they are very similar. The FP code inspects the `Calculation` element it is passes to choose the right action to take. The OO implementation breaks out the actions into their own method, and relies on the caller to make the choice for them.

The relationship is this: each constructor in the FP implementation becomes a method in the OO implementation. This removes the need for the pattern matching. The transformation is known as *Church encoding*.

We can go the opposite way as well: convert every method in the OO representation into a case class in a sealed trait (an algebraic data type), and then use a pattern match to chose the action to take (a structural recursion). This transformation is known as *reification*.


### Unification

We've seen that the OO and FP representations have different types of extensibility, and we can transform between the two representations using Church encoding or reification.
   
This means we can keep one mental model and choose the representation that is best for the problem at hand. We can think of OO and FP not as entirely different programming paradigms but choices we make to encode a solution to the problem according to the tradeoffs we want to make.


## Case Study

Let's see an example where where this transformation is useful. One aspect we haven't considered yet is performance. In the FP representation we must allocate memory to hold the data structure that represents the operations we want to perform. In the OO representation we don't have that allocation. This can be advantageous.

We were recently engaged to develop a time series analysis system for [Maana][maana], the Seattle enterprise knowledgement management startup. This system had some fairly stringent performance demands that justified implementing a custom system rather than using off-the-shelf software like Spark.

The nice thing about time series is they have a well defined order, and algorithms always respect that order. So we can implement the foundation of the system as a stream processing engine that works over the data from beginning to end, whether it is arriving in real-time or being streamed from disk.
  
There is a lot of prior work on the kind of API for this system. Spark, Monix, FS2, and Akka Streams all provide examples. We don't need the rich API of these system in our implementation, and we have some methods that are particular to time series, but it provides a good mental model for what we'll be talking about.
  
In this kind of system we create a directed acyclic graph (DAG) representing what we want to perform, and then run it when we've finished the description. This is the classic FP model of separating describing what you want to do from carrying it out. Nodes in our graph represent things like resampling the time series, or restricting it to a particular time range.

Our system has a pull based implementation model. Data flows from upstream to downstream. The most downstream node is the root, and the root pulls data through the system by requesting data from the nodes immediately upstream. They in turn request data from nodes immediately upstream from them, and this happens recursively till leaf nodes are reached. Leaf nodes then send data downstream, which gets transformed along the way until it reaches the root.
  
We don't just pass back data; we need to include some control information as well. We might have run out of data, we might be waiting for more (e.g. if we've filtering out certain time ranges), or we might have encountered an error. We can use this representation:

```scala
sealed trait Result[+A]
final case class Emit[A](get: A) extends Result[A]
final case object Waiting extends Result[Nothing]
final case object Complete extends Result[Nothing]
final case class Error(reason: ErrorType) extends Result[Nothing]
```

The problem here is we allocate a lot. We allocate a `Result` for every node in the DAG and for every data element we process. Usually we process a lot more elements than we have nodes in the graph.
  
We can use the Church encoding to reduce the allocation! Instead of returning a `Result` we can call a `Receiver` with the correct method, where `Receiver` is the Church-encoding of `Result`.

```scala
trait Receiver[A] {
  def emit(a: A): Unit
  def waiting(): Unit
  def complete(): Unit
  def error(reason: ErrorType): Unit
}
```

The action performed by a given node is constant so we can allocate one receiver per node in the graph and completely eliminate per-element allocation.

## Benchmarks

Sounds good in theory, but how does it work out in practice? On [Github](https://github.com/noelwelsh/church-and-state) I have a simplified implementation that demonstrates the FP and the Church-encoding representations. 
For a simple benchmark I get the following results

```
termination.StreamBenchmark.zipAndAdd avgt 200 532.190 ± 2.724 ms/op
partial.StreamBenchmark.zipAndAdd     avgt 200 387.252 ± 2.165 ms/op 
```

where `termination` is the FP style, and `partial` is the Church-encoded representation.

The Church encoded representation is 1.4x faster. This is a great improvement from a simple program transformation we can apply in a very systematic way.

### Continuation Passing Style

You may have noticed we have changed from *returning* a `Result` to *calling* a `Receiver`. Essentially we've inverted our control-flow. How does this effect our code? This is another program transformation, known as continuation-passing style (CPS). Writing code in continuation passing style is not especially hard---it's a well defined transformation, just like Church encoding is well defined---and it's something that people who use callback-heavy APIs, such as Node.js programmers, do all the time.

### Conclusions

In summary we Church-encoded our `Result` type, and then CPSed the code that used it. In fact we only partially did this. If you look in the Github repository you'll see examples that are fully Church encoded as well. This is an important point: it's not all or nothing with these techniques. You can apply them to a small part of your code or the entire system.
   
The result for us was a big performance improvement by avoiding excessive allocation. This is important to achieve our performance goals. Usually optimisation means writing lots of nasty code. I wouldn't say the code we ended up with is paritcularly beautiful but it is related to the clearer original code by two systematic transformations: Church encoding and continuation passing style. We can easily reverse these transformations, if only in our head, to get back to the code that is easier to work with though less performant.


## Type classes and Free structures

I now want to expand a bit more on the idea of Church encoding being a unifying idea.

Let's look at a typical type class. Here's how `Monad` might look in something like Cats (the actual implementation also has `tailRecM`, which I've removed for clarity.)

```scala
trait Monad[F[_]] {
  def flatMap[A,B](fa: F[A])(f: (A) =. F[B]): F[B]
  def pure[A](x: A): F[A]
}
```
  
This looks a lot like a Church encoding. It is! But what is it a Church encoding of? If we reify it (remember reification is the opposite of Church encoding) we get something like

```scala
sealed trait Monad[F[_],A]
final case class FlatMap[F[_],A,B](fa: Monad[F,A], f: A => Monad[F,B]) extends Monad[F[_],B]
final case class Pure[F[_], A](x: A) extends Monad[F[_],A]
```
  
This is the free monad! (The usual encoding of the free monad is slightly different for reasons of efficiency but the concept is the same.)

So type classes are Church encodings of free structures, or alternatively, free structures are reifications of type classes.


## Extensibility

Earlier I said OO style makes it easy to add new operations while FP makes it easy to add new actions. But with type classes we can do both. We can extend a type class with new operations (such as `Monad` being an extension of `Applicative`), which is OO style extension. We can also add a new implementation of a type class for a given type (think of the many `Monoid` instances for `Int`), which is FP style extension. Did I lie to you?

Dear reader, I would never knowingly lie to you. What has happened here is we've snuck in an extra degree of abstraction over the basic OO code I showed earlier. It's the type parameter `F` in the definition of `Monad` below.
  
```scala
trait Monad[F[_]] {
  def flatMap[A,B](fa: F[A])(f: (A) ⇒ F[B]): F[B]
  def pure[A](x: A): F[A]
}
```

We can apply the same trick to `Calculator`. If we add a type parameter to represent the output type we can now implement different actions, such as pretty printing which returns a `String`.

```scala
trait Calculator[A] {
  def literal(v: Double): A 
  def add(a: A, b: A): A 
  def subtract(a: A, b: A): A 
  def multiply(a: A, b: A): A 
  def divide(a: A, b: A): A 
}

object PrettyPrinter extends Calculator[String] {
  def literal(v: Double): String = v.toString
  def add(a: String, b: String): String = s"($a + $b)"
  def subtract(a: String, b: String): String = s"($a - $b)"
  def multiply(a: String, b: String): String = s"($a * $b)"
  def divide(a: String, b: String): String = s"($a / $b)"
}
```
   
When we use a `Calculator` we should delay the choice of concrete implementation so we can plug in different implementations depending on our needs. This is the separation between describing what we want to occur and carrying it out.

```scala
def expression[A](c: Calculator[A]): A = {
  import c._

  add(literal(1.0), subtract(literal(3.0), literal(2.0)))
}

expresssion(PrettyPrinter)
// res: String = (1.0 + (3.0 - 2.0))
```
   
If we do this we have basically implemented a *tagless final* interpreter (sometimes also known as a finally tagless interpreter, or an object algebra in the OO world). If we go the opposite direction, and reify to a free structure, we can use the `Inject` typeclass to regain the flexibility we've lost. This is known as *data types à la carte* style.
   
So we see that tagless final style is effectively a Church encoding of data types a la carte style, or vice versa.


## Summary and Further Reading

Let's run down what we've seen:

 - The Church encoding allows us to transform FP style to OO style
 - Reification allows us to transform OO style to FP style
 - We can choose a style due to the extensibility we want, or due to performance demands
 - Type classes are Church encodings of free structures. Free structures are the reification of type classes.
 - We can get extensibility in both directions using tagless final style or data types a la carte style. Tagless final is a Church encoding of data types a la carte.
 
Hopefully you now agree that the Church encoding is a useful tool for unifying functional and object-oriented programming!

This post is based on a talk I gave at Scala Days Copenhagen. [My slides][slides] are also available.
They have more pictures and fewer words.

I'd like to end with some more general thoughts.
Firstly, I've presented the Church encoding in a fairly matter-of-fact style, but it's taken me quite a while to piece everything together.
The pieces are all in the academic literature, and I want to present some of them below, along with some commentary, so others can follow this path.
I hope this will make navigating the literature a bit easier for those who are interested, and also show that there are many great ideas to be found out there---though the presentation is often a bit hard to parse for the working programmer[^bananas].
Although I've tried to make my survey reasonably complete I have no doubt missed some important works; I'm not an academic and this is a blog post, not a peer-reviewed publication.

The idea of the two orthogonal axes of extensibility (which I called operations and actions) is something I first read in [Synthesizing Object-Oriented and Functional Design to Promote Re-Use][synthesizing]. The challenge of allowing extensibility along both axes is known as the [Expression Problem][expression].

In this post I discussed two solutions to the expression problem: [tagless final interpreters][tagless-final] and [data types a la carte][a-la-carte]. 
In the OO world tagless final interpreters are known as [object algebras][object-algebras]. 
[This fantastic blog post][object-algebra-to-finally-tagless] shows the correspondence.
Note that tagless final style is sometimes known as finally tagless.

It's relatively rare for real programs to require a solution to the expresion problem, so I'm not advocated coverting all code to one of the two styles above, but it does come up in practice.
For example, in [Doodle][doodle] I want to be able to render to different backends (adding actions) and support operations specific to a particular platform (adding operations).
In Scala the tagless final stye is often easier to work with.
There are a number of [type inference][9879] [bugs][5195] involving generalised algebraic data types (GADTs) in Scala that are needed when using FP style.
Tagless final is also a bit more familiar to OO programmers, who make up most of the programmer population at this point in time.

More on the relationship between data types a la carte style and tagless final style is given in [Folding Domain-Specific Languages: Deep and Shallow Embeddings][folding-dsl].
(Reified or FP style is also known as a deep embedding, while Church encoded or OO style is a shallow embedding).
I found this paper very interesting and quite easy to read.
Note that this paper calls the relationship a Boehm-Berarducci encoding.
While this is strictly correct---the Church encoding is between data and the untyped lambda calculus (which Alonzo Church invented)---I believe the essential idea is the same.
If you want to know about the Boehm-Berarducci encoding there is [more here][beyond-church].
Also note that this paper is a "Functional Pearl", a specific type of paper published at the International Conference on Functional Programming (ICFP).
I find that the Functional Pearls are often the most useful papers at ICFP.
For example, the [paper introducing Applicatives][idioms] (called Idioms at the time) is a Functional Pearl.

It is very easy to explore the academic literature if you go to [scholar.google.com](https://scholar.google.com/) and enter the name of a paper that interests you.
You can then chase citations forward and backward to time to get more context.

Finally, although I've presented this as a unification of FP and OO, it is purely a unification of FP and OO *programming techniques*, not a unification of the FP and OO *programming paradigms*.
I presented on the two paradigms at [Scala Days last year][structure].
I'm still working solidly in the functional programming paradigm: avoiding mutable state, emphasising static reasoning, and so on.
 
[alonzo-church]: https://en.wikipedia.org/wiki/Alonzo_Church
[maana]: http://maana.io/
[slides]: /files/noelwelsh-scala-days-copenhagen-church-and-state.pdf
[expression]: http://homepages.inf.ed.ac.uk/wadler/papers/expression/expression.txt
[synthesizing]: https://cs.brown.edu/~sk/Publications/Papers/Published/kff-synth-fp-oo/
[tagless-final]: http://okmij.org/ftp/tagless-final/course/lecture.pdf
[a-la-carte]: http://www.cs.ru.nl/~W.Swierstra/Publications/DataTypesALaCarte.pdf
[object-algebras]: https://www.cs.utexas.edu/~wcook/Drafts/2012/ecoop2012.pdf
[object-algebra-to-finally-tagless]: https://oleksandrmanzyuk.wordpress.com/2014/06/18/from-object-algebras-to-finally-tagless-interpreters-2/
[doodle]: https://github.com/underscoreio/doodle/
[9879]: https://github.com/scala/bug/issues/9879
[5195]: https://github.com/scala/bug/issues/5195
[folding-dsl]: http://www.cs.ox.ac.uk/jeremy.gibbons/publications/embedding.pdf
[beyond-church]: http://okmij.org/ftp/tagless-final/course/Boehm-Berarducci.html
[structure]: https://www.youtube.com/watch?v=bL-CcjKW1lw
[idioms]: http://strictlypositive.org/Idiom.pdf

[^bananas]: All of this literature is reasonably readable, in my opinion. A paper that I consider both spectacularly unreadable but still important is [Functional Programming with Bananas, Lenses, Envelopes and Barbed Wire ](http://doc.utwente.nl/56289/1/meijer91functional.pdf).

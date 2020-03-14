---
layout: post
title: Reification, Kleislis, and Stream Libraries
author: Noel Welsh
---

A major theme of this blog is design principles for Scala code. In the past we've talked about Scala mechanics like [sealed traits][sealed-traits] and general principles like [simplicity in Scala][keep-scala-simple]. In this post I want to discuss a very general principle called *reification* and show its application in two different domains: monad composition and stream libraries like Akka Stream.

<!--more-->

First, what the heck does reification mean? It's a fancy word meaning to make concrete what was previously an abstract idea. Specifically in programming, reification means to take some concept that was previously implicit in the code and turn it into data that can then manipulated by the program. Consider functions, for example. In older procedural and OO languages (think Java prior to Java 8) functions (also known as procedures or methods) are not data. You cannot pass a function to a function, and you cannot return a function from a function. Functional programming reifies this concept, making them data that you can pass around your code and manipulate like other data. As Scala programmers we know the benefit that reifying functions brings.

A classic use of functions is to build up a chain of processing steps, using function composition, that can then be applied to transform some input. This could be a web service, as described in [Your Server as a Function][your-server-as-a-function], an [ETL][etl] pipeline running on Spark or Flink, or perhaps even a sequential Monte Carlo process for pricing financial instruments. Conceptually these all consist of composing together some processing steps to form a processing pipeline. In code

~~~scala
val pipeline: A => D =
  ((a: A) => b) andThen
  ((b: B) => c) andThen
  ((c: C) => d)
~~~

where the input type `A` could be a `HttpRequest`, and line of a log file, or a sample from the prior distribution as appropriate, and the output type `D` is an `HttpResponse`, processed data, or a sample from the posterior distribution. 

Now we can simply apply this function to our data in the usual way.

~~~scala
val result: D = pipeline(input)
~~~

By reifying the concept of a function we can build a reusable library of pipeline stages. For example, in our web service we will probably build a reusable authentication component that sits at the front of every pipeline. We can also use our pipelines in different situations. In a test case, we could map our web service over a list containing test data, for example. 

That's all fairly standard stuff. Now let's get a bit more interesting. Our pipeline stages will probably return monads. Instead of abstract type `A => B` they will be `A => F[B]` where `F` is some monad---perhaps `Future` for the webservice, `Either` or equivalents to represent errors in our ETL pipeline, and a [probability density monad][ppp-with-monads] in our Monte Carlo program. When we try to compose these together things become a bit trickier. Our pipeline definition becomes

~~~scala
val pipeline: A => F[D] =
  (a: A) =>
    ((a: A) => b.point[F])(a) flatMap
    ((b: B) => c.point[F]) flatMap
    ((c: C) => d.point[F])
~~~

where the `point` method creates a monad instance from a value.

This is easier to read if we break out the definition of the stages from their composition. 
Given stages

~~~scala
val stageA = (a: A) => b.point[F]
val stageB = (b: B) => c.point[F]
val stageC = (c: C) => d.point[F]
~~~

to compose these stages we need to write

~~~scala
val pipeline: A => F[D] =
  (a: A) => stageA(a) flatMap stageB flatMap stageC
~~~

It's not awful but it is certainly not as clean as the function composition we started with. The issue is we have an implicit concept of composition of `A => F[B]` functions (which is the type of function we pass to `flatMap`) but we have not reified that concept in our code. It turns but the reification already exists and is known as a [Kleisli][cats-kleisli]. By using a Kleisli in our code we can write

~~~scala
val pipeline: Kleisli[F, A, B] =
  Kleisli((a: A) => b.point[F]) andThen
  Kleisli((b: B) => c.point[F]) andThen
  Kleisli((c: C) => d.point[F])
~~~

and regain a style that matches the function composition we started with. Using a Kleisli makes it simple to create and compose pipeline stages that return monads.

You can see the same design principle at work in [Akka Streams][akka-stream] compared to [ReactiveX][reactivex] inspired implementations like [Monifu][monifu]. Event streams are monads in the ReactiveX model and there is no explicit representation of a processing stage separate from the event stream it operates on. Specifying a pipeline of stages separately from the event stream they operate over is exactly the same problem that we saw above and solved with Klieslis. Akka Streams reifies the concepts of a processing stage, called a [`Flow`][flow] in their terminology, which is analogous to a Kleisli. This allows you to create reusable processing pipelines exactly equivalent to those we discussed above using function and Kleisli composition.

We've seen a simple example of how reifying a concept improves code. Are there other examples? Unsurprisingly, there are a great many. In fact, it's one of the main design tools used in functional programming. Whenever we write an [abstract syntax tree][free-monads-are-simple] or use the [interpreter pattern][runar-interpreter-pattern] we are reifying a concept as data. The interpreter pattern is in turn the big idea behind [Doodle][doodle], the [financial contracts library][financial-contracts] that [LexiFi][lexifi] is founded on, Facebook's [Haxl][haxl], Instagram's [feature gating library][instagram-feature-gate], and a whole heap more.

[sealed-traits]: {% post_url 2015-06-02-everything-about-sealed %}
[keep-scala-simple]: {% post_url 2015-06-25-keeping-scala-simple %}
[free-monads-are-simple]: {% post_url 2015-04-14-free-monads-are-simple %}

[your-server-as-a-function]: http://monkey.org/~marius/funsrv.pdf
[etl]: https://en.wikipedia.org/wiki/Extract,_transform,_load
[ppp-with-monads]: http://mlg.eng.cam.ac.uk/pub/pdf/SciGhaGor15.pdf
[cats-kleisli]: https://non.github.io/cats//tut/kleisli.html

[reactivex]: http://reactivex.io/
[akka-stream]: http://doc.akka.io/docs/akka-stream-and-http-experimental/1.0/scala.html
[monifu]: https://github.com/monifu/monifu
[flow]: http://doc.akka.io/api/akka-stream-and-http-experimental/1.0/#akka.stream.scaladsl.Flow 

[runar-interpreter-pattern]: https://www.youtube.com/watch?v=hmX2s3pe_qk
[doodle]: https://github.com/underscoreio/doodle
[financial-contracts]: http://research.microsoft.com/en-us/um/people/simonpj/Papers/financial-contracts/contracts-icfp.htm
[lexifi]: https://www.lexifi.com/
[haxl]: https://github.com/facebook/Haxl
[instagram-feature-gate]: http://engineering.instagram.com/posts/496049610561948/flexible-feature-control-at-instagram

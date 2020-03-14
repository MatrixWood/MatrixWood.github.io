---
layout: post
title: An Introduction to Cats
category: programming
repost: underscore
---

You wouldn't think the Internet would need a an [introduction to cats][cat-neuron], but then this [Cats][cats] is a Scala library, not a small furry bed-hogging mammal. Cats is the spiritual successor to Scalaz: a library of absolutely essential utilities you really want to be using in your Scala code. Compared to Scalaz, Cats is more modular and it is using some newer tools to make its code base easier to work with.

Cats is still a closer to being a kitten than the king of the alley, but it has definitely reached the stage where it is usable. In this article I'll provide a basic introduction to getting started with Cats.

<!--more-->

## Getting Cats

A [snapshot release][cats-sonatype] of Cats is now available on Sonatype. Cats, as I mentioned above, is highly modularised, so there are some dependencies you need for the full Cats experience:

- [`algebra`][algebra], which defines basic abstractions like `Monoid` and `Semigroup`;
- `cats-core`, which defines the basic abstractions in Cats;
- `algebra-std`, for implementations of the abstractions in `algebra` for types in the standard library; and
- `cats-std`, which has implementations of the abstractions in Cats for the standard library.

That sounds like a lot of stuff, but you can just copy and paste the snippet below in your `build.sbt` to get going.

*Also note: to run some of the examples below you will need to clone `cats` from Github and run `sbt publish-local`.* Cats is moving fast and the snapshot release lags behind the development head.

~~~ scala
val snapshots = "Sonatype Snapshots"  at "https://oss.sonatype.org/content/repositories/snapshots"

val algebraVersion = "0.2.0-SNAPSHOT"
val catsVersion    = "0.1.0-SNAPSHOT"

val algebra    = "org.spire-math" %% "algebra" % algebraVersion
val algebraStd = "org.spire-math" %% "algebra-std" % algebraVersion

val cats       = "org.spire-math" %% "cats-core" % catsVersion
val catsStd    = "org.spire-math" %% "cats-std" % catsVersion

scalaVersion := "2.11.6"

libraryDependencies ++=
  Seq(
    algebra, algebraStd,
    cats, catsStd
  )

resolvers += snapshots
~~~ 

## Using Cats

Cats has a fairly straightforward organisation.

In the package `cats` you will find the basic types. Cats reexports some of the types from `algebra`, which provides very basic facilities like `Order` and `Monoid`.

In `cats.data` there are data types that work with the abstractions defined in `cats`. These are the tools you are most likely to use in your day-to-day work.

The package `cats.std` holds type class instances for classes in the standard library, such as `Option`, `List`, and `Map`.

Finally, `cats.syntax` contains useful syntax (implicit classes) for working more easily with Cats.

## Cats.Data

The types in `cats.data` are probably the first place you'll explore when you start using Cats. If you've used Scalaz before you will find analogues to many familiar abstractions. Here are some of the highlights.

### Xor

`Xor[A, B]` is a right-biased `Either`. An instance of `Xor` is either a `Xor.Left[A]` or a `Xor.Right[B]`.  Being right-biased means that the `Right` case is considered a success, and `Left` is considered failure. Unlike `Either`, `Xor` has `flatMap` and `map` methods. These methods do something if the actual instance is `Right`, in the same way that `flatMap` on `Option` only does something if the actual instance is a `Some`. This choice gives rise to the term "right-biased".

To work better with type classes, the `Xor` companion object provides convenience constructors `left` and  `right` that return results of type `Xor`. These are the preferred way to construct instances.

Here's an example.

~~~ scala
import cats.data.Xor

object XorExample {
  // Get Xor.left and Xor.right into scope
  import Xor.{left, right}

  // The type we will be working with
  type Result[A] = String Xor A
  val l: Result[Int] = left("Failed")
  val r: Result[Int] = right(1)

  // Nothing happens when we map a left
  println(l map (x => x + 1))
  // The right is transformed
  println(r map (x => x + 1))
}
~~~

`Xor` is a great type to represent the results of some computation, using `Left` to hold an error message and `Right` to hold a successful result.

### Validated

The `Validated[E, A]` type is the equivalent of Scalaz's `Validation`. Like `Xor` it has two cases, which are `Validated.Invalid[E]` and `Validated.Valid[A]`. `Validated` is not a monad, so it does not have a `flatMap` method. It is, however, an applicative functor, and its typical usage is to accumulate errors. 

Whereas an `Xor` stops running as soon as it encounters a `Left`, when we combine `Validated` instances the error messages contained within any `Invalid` instances will be appended together in the final result.

Like `Xor`, `Validated` has convenience constructors defined on the companion object.

The standard way to combine `Validated` instances is using the `|@|` syntax. Here's an example.

~~~ scalated, Xor}
import cats.syntax.apply._ // For |@| syntax
import cats.std.list._ // For semigroup (append) on List

object ValidatedExample {
  import Xor.{left, right}
  import Validated.{invalid, valid}

  // We are going to compare the behaviour of Xor and Validated.  First we
  // define some instances. Then we combine them using flatMap
  // (for-comprehension) or `|@|` as appropriate.

  type Error = List[String]
  type XorR  = Xor[Error, Int]
  type ValidatedR = Validated[Error, Int]

  val x1: XorR = right(1)
  val x2: XorR = left(List("Stops here"))
  val x3: XorR = left(List("This will be ignored"))

  val v1: ValidatedR = valid(1)
  val v2: ValidatedR = invalid(List("Accumulates this"))
  val v3: ValidatedR = invalid(List("And this"))

  // Stops as soon as we encounter an error
  println(
    for {
      x <- x1
      y <- x2
      z <- x3
    } yield x + y + z
  )

  // Accumulates all the errors
  println(
    (v1 |@| v2 |@| v3) map { _ + _ + _ }
  )
}
~~~

`Validated` and `Xor` are the two most commonly used data types in `cats.data`. Let's look at the other packages now.

## Cats

The base `cats` packages contains the fundamental types like `Monad` and `Applicative`. If you're used to Scalaz there are a few differences here. Cats defines a `FlatMap` trait that `Monad` extends. Like Scalaz, `Applicative` extends `Apply`, and it is `Apply` that defines the `|@|` syntax we used when we looked at `Validated`. 

## Cats.Syntax

Cats defines syntax is a separate package, as in Scalaz. The tools in this package enrich types with extra methods that makes working with Cats more convenient. We have already seen an example when we imported `cats.syntax.apply._` to get the `|@|` syntax. There are similar helpers for `monad`, `eq`, and other types. If you're used to Scalaz you will note some surprising omissions, such as the lack of syntax for constructing `Xor` instances with `.left` and `.right`. This is one area where Cats shows its youth -- these helpers are still under development. 

## Cats.Std

In `cats.std` are the type class instances for the standard library. Import `cats.std.list._` to get the `Monad`, `Semigroup`, and other instances defined on `List`. Instances defined on the value types are in `cats.std.anyval._`

## Final Thoughts

This has been a very quick overview of Cats. There are other packages, like `cats.free`, that are also part of the project, and of course it is still changing rapidly. The best place to track the development of Cats is its very active [gitter Room][cats-gitter].

Cats also has an emphasis on accessibility that I found lacking from Scalaz. [Basic documentation][cats-docs] is available, with more under development, and Cats also subscribes to the [Typelevel code of conduct][conduct] to ensure a welcoming environment for all.

[cats]: https://github.com/non/cats
[cats-sonatype]: https://oss.sonatype.org/content/repositories/snapshots/org/spire-math/
[algebra]: https://github.com/non/algebra
[conduct]: http://typelevel.org/conduct.html
[cat-neuron]: http://www.eetimes.com/document.asp?doc_id=1266579
[cats-gitter]: https://gitter.im/non/cats
[cats-docs]: http://non.github.io/cats/

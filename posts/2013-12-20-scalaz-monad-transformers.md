---
layout: post
title: "Scalaz Monad Transformers"
description: ""
category: programming
lead: Do you like monads? Do you like monads in your monads? If so, you'll love monad transformers. If you like monads in your monads, you like Scala, and you want to learn more about monad transformers then read on &hellip;
repost: underscore
---

Monad transformers allow us to stack monads. Say we have a monad, like `Option`, and we want to wrap it in another monad, like `\/`, in a convenient way (where convenient is to be defined shortly). Monad transformers let us do this. Scalaz comes with lots of monad transformers. Let's see how to use them and the benefits they supply.

## Example

Let's motivate monad transformers with the above example. We have an operation that may or may not generate a value. Something like getting a value from a database. Thus we model it as an `Option`. This operation may also encounter various other errors. To keep things simple we'll encode the errors as `String`s. So we basically have three different types of result:

1. value found;
2. value not found; or
3. error occurred.

We can model this as `type Result[+A] = String \/ Option[A]`. (Note that `\/` is Scalaz's version of `Either`.)  We can construct values of this type by hand:

``` scala
import scalaz._
import Scalaz._

type Result[+A] = String \/ Option[A]

val result: Result[Int] = some(42).right
// result: Result[Int] = \/-(Some(42))
```

To use `result` we must unwrap it twice, which is tedious:

``` scala
val transformed =
  for {
    option <- result
  } yield {
    for {
      value <- option
    } yield value.toString
  }
// transformed: scalaz.\/[String,Option[String]] = \/-(Some(42))
```

This is fairly horrible. The desugared version is actually a bit clearer:

``` scala
val transformed = result map { _ map { _.toString } }
// transformed: scalaz.\/[String,Option[String]] = \/-(Some(42))
```

Still, can't we avoid this nesting? It seems like we should be able to chain calls to `flatMap` and friends and just have it do the right thing. With monad transformers we can. Here's the transformed version.

``` scala
type Error[+A] = \/[String, A]
type Result[+A] = OptionT[Error, A]

val result: Result[Int] = 42.point[Result]
val transformed =
  for {
    value <- result
  } yield value.toString
```

There are three changes:

1. the type definitions;
2. the way we construct values of these new types; and
3. the removal of one layer of nesting when we use the monad.

Let's go through these in order.

## Type Definitions

The type `OptionT[M[_], A]` is a monad transformer that constructs an `Option[A]` inside the monad `M`. So the first important point is the monad transformers are built from the inside out.

Note that I define a type alias `Error` for the monad we wrap around the `Option`. Why is this the case? It has to do with type inference. `OptionT` expects `M` to have a type (technically, a kind) like `M[A]`. This is, `M` should have a single type parameter. `\/` has two type parameters, the left and the right types. We have to tell Scala has to get from two type parameters to one. One option is to use a type lambda:

``` scala
type Result[A] = OptionT[{ type l[X] = \/[String, X] }#l, A]
```

Clearly this horror should not be inflicted on the world. A saner option is to just define the type as I have done above with `Error`.

## Constructing Values

Constructing values of `Result` can be done in a variety of ways, depending on what we want to achieve.

If we want to construct a value in the default way, we can use the `point` method like so:

``` scala
val result: Result[Int] = 42.point[Result]
// result: Result[Int] = OptionT(\/-(Some(42)))
```

Note that this wraps our value in `Some` and a right `\/`.

What if we want, say, a `None` for the option. We can't use `point` as we have above:

``` scala
None.point[Result]
// Result[None.type] = OptionT(\/-(Some(None)))
```

The solution is to use the `OptionT` constructor:

``` scala
val result: Result[Int] = OptionT(none[Int].point[Error])
// result: Result[Int] = OptionT(\/-(None))
```

Here I've used Scalaz's `none`, and `point` on `Error` to construct the value of the correct type, before wrapping it in an `OptionT`.

If we want to create a left `\/` we go about it the same way:

``` scala
val result: Result[Int] = OptionT("Error message".left : Error[Option[Int]])
// result: Result[Int] = OptionT(-\/(Error message))
```

Note the type declaration, needed to assist type inference.


## Using the Monad

When we use our new monad `map` and `flatMap` do multiple levels of unwrapping for us.

``` scala
val result = 42.point[Result]
result.map(x => x + 2)
// scalaz.OptionT[Error,Int] = OptionT(\/-(Some(44)))
result.flatMap(_ => "Yeah!".point[Result])
// scalaz.OptionT[Error,java.lang.String] = OptionT(\/-(Some(Yeah!)))
```

Of course we might not want to unwrap all the layers of our monad. We can manually unwrap our data if need be. All monad transformers in Scalaz return their data if you call the `run` method. With this we can do whatever we want, such as folding over the `\/`.

``` scala
val result = 42.point[Result]
result.run
// Error[Option[Int]] = \/-(Some(42))
result.run.fold(
  l = err => "So broken",
  r = ok  => "It worked!"
)
// java.lang.String = It worked!
```

What about some utility functions to help with this? There are no such methods defined for all monad transformers, that I know of, but in the particular case of `OptionT` we can use the `flatMapF` method. For a type `Option[F[_], A]` the normal `flatMap` has type

``` scala
flatMap[B](f: A => OptionT[F, B]): OptionT[F, B]
```

whereas `flatMapF` has type

``` scala
flatMapF[B](f: A => F[B]): OptionT[F, B]
```

(Note I removed an implicit parameter from the method signatures above.)

For our `Result[Int]` type this means the parameter `f` to `flatMap` should have type `Int => \/[String, B]`. Note that `B` is *not* wrapped in an `Option`; `flatMapF` will do this for us. We also don't have to wrap our result in `OptionT`.

Here is an example:

``` scala
// this is a fairly silly function, but it serves as an example
def positive(in: Int): \/[String, Boolean] =
  if(in > 0)
    true.right
  else
    "Not positive".left

val good = 42.point[Result]
good flatMapF positive
// scalaz.OptionT[Error,Boolean] = OptionT(\/-(Some(true)))

val bad = -3.point[Result]
bad flatMapF positive
// scalaz.OptionT[Error,Boolean] = OptionT(-\/(Not positive))
```


# Conclusion

Once you start using monads it's quite easy to find yourself using a lot of them. For example, if you do asynchronous programming in Scala you are likely using `Future` which is a monad[^scalaz-contrib]. Debugging futures can be hard. Futures don't give good stack traces due to the continual context switches, and logs are hard to interpret as output from many threads is mixed together. An alternative is to keep an in-memory log with each computation running in a `Future`, and output this log when the `Future` finishes. This can be accomplished using the `Writer` monad. As soon as you do this you have a stack of monads, and monad transformers become useful. Once you know what to look for you'll find them everywhere!

[^scalaz-contrib]: See the [scalaz-contrib](https://github.com/typelevel/scalaz-contrib) package for `Monad` instances for `scala.concurrent.Future`.

---
layout: post
title: "Monadic IO: Laziness Makes You Free"
author: Noel Welsh
category: programming
repost: underscore
---

Understanding monads is a puzzle with many parts. Understanding the monad interface was easy enough for me, as I'd been programming in functional languages for a while when I first started exploring them, but for a long time I didn't understand how they made IO operations pure. The answer is to add an extra wrinkle, usually glossed over in Haskell oriented sources, by making all IO actions lazy. It this article we're going to explore how this works and undercover some surprising relationships to the free monad, which we have been covering in [recent][free-monad-interpreter] [posts][free-monad-deriving].

<!--more-->

We start with a very simple Scala program.

~~~ scala
println("Monads are the best!")
~~~

We know that `println` is impure, meaning it breaks substitution. We can't substitute a call to `println` with the result of that call (`()`) without changing the semantics of our program. This is in contrast to pure programs, such as `1 + 2 + 3`, which we can freely substitute.

Concretely

~~~ scala
println("Monads are the best!")
~~~

is not equivalent to

~~~ scala
()
~~~

whereas

~~~ scala
1 + 2 + 3
~~~

is equivalent to

~~~ scala
6
~~~

Our goal is to implement `println` in such a way that its return value is something that does allow for substitution. I've already hinted we need a monad. But how exactly should this monad be implemented? The solution comes back to the idea we talked about when [introducing the free monad][free-monad-interpreter]: separate the representation and the interpreter.

What we're going to do is make `println` return an action that, when we run it, really does the printing. An implementation like this will do:

~~~ scala
object Pure {
  def println(msg: String) =
    () => Predef.println(msg)
}
~~~

`Predef.println` is the "real" `println` that prints to the console. Our implementation, within `Pure`, returns a function of no arguments (a thunk) that calls the real `println` when applied. We can freely substitute calls to `Pure.println` with the result of calling `Pure.println`. In other words

~~~ scala
Pure.println("Monads are the best!")
~~~

is equivalent to

~~~ scala
() => Predef.println("Monads are the best!")
~~~

We have an implementation that maintains substitution, which is a step forward. But to make this implementation useful we need two more things: 

- we must somehow tie together our uses of `println`, so we don't just have thunks randomly littered throughout our code; and
- at some point we must actually run these thunks to print stuff out.

We can solve both these problems by defining a monadic API for IO actions. This involves changing the representation from a function to a data type with an implementation of `flatMap` and a method `run`. I recommend undertaking this exercise yourself. It's a bit more subtle than I thought it would be; the main issue is defining `flatMap` without running actions prematurely.

Here's an example to get you started. Your code *should not* print anything until `Example.run` is called.

~~~ scala
object Example {
  val io =
    for {
      _ <- Pure.println("Starting work now.")
      // Do some pure work
      x = 1 + 2 + 3
      _ <- Pure.println("All done. Home time.")
    } yield x

  def run =
    io.run
}
~~~

Here's my solution:

~~~ scala
object Pure {
  sealed trait IO[A] {
    def flatMap[B](f: A => IO[B]): IO[B] =
      Suspend(() => f(this.run))

    def map[B](f: A => B): IO[B] =
      Return(() => f(this.run))

    def run: A =
      this match {
        case Return(a) => a()
        case Suspend(s) => s().run
      }
  }
  final case class Return[A](a: () => A) extends IO[A] 
  final case class Suspend[A](s: () => IO[A]) extends IO[A]

  object IO {
    def point[A](a: => A): IO[A] =
      Return(() => a)
  } 

  def println(msg: String): IO[Unit] =
    IO.point(Predef.println(msg))
}
~~~


The key problem, as I mentioned above, is implementing `flatMap`. With an object of type `IO[A]` and a function of type `A => IO[B]`, it seems that we must run the `IO[A]` to get the `A` to apply to the function. However, once we run actions we break substitution. The solution is to introduce a separate case to our algebraic data type to represent a call to `flatMap` without actually running anything. This is the `Suspend` case in my implementation.

Avid readers of the blog will recognise that this is almost exactly the algebraic data type we used when [deriving the free monad][free-monad-deriving]! That's no accident. The free monad is all about [separating the structure of the computation from the interpreter that gives it meaning][free-monad-interpreter]. *We are doing exactly the same thing here.* The structure of the computation is represented by the algebraic data type, and we give meaning to the structure when we `run` it.

This is a fairly simple example, but I found a surprising number of lessons in it.

The first is the trick of making IO pure by *not doing any IO* (until we `run` the code). This trick is very useful. It's the same implementation technique used in Scalaz's [`Task`] (a better alternative to `Future`) and in the free monad.

We can't maintain substitution after we run our IO actions. The way Haskell handles the `IO` monad is to only allow the runtime to run it (with the exception, I believe, of `unsafePerformIO`). Therefore all programs are pure from the programmer's point of view. In Scala we just have to be careful to separate the two phases so we don't try to use substitution of actions after they've been run.

The idea of representing actions as data is very general. I've explored this point in depth in the [prior post][free-monad-interpreter] introducing the free monad. We've seen another example here. We're also seeing the same implementation pattern come up again. So as a general point, if you find yourself implementing some monad variant and you don't want to use the free monad, you probably need an algebraic data type like the one we just saw.

In some sense the representation of monads in terms of `map` and `join` (described [previously][free-monad-interpreter]) is more primitive than the one in terms of `flatMap`. With this representation we build nested structured like `IO[IO[IO[C]]]` through repeated application of `map`, and we then reduce these back to just, say, `IO[C]` using `join`. We can view `map` as sequencing actions to perform, and `join` as performing them. 

Finally, we can derive a useful lesson about monad composition in the free monad. If you know about monad transformers, you'll know they are one approach to composing monads. It's fairly common to define a "monad stack" that is used consistently throughout an application. For example, an application I'm currently writing uses

~~~ scala
type Result[Error, Success] = EitherT[Task, Error, Success]
~~~

The free monad offers another approach to monad composition, via the [a la carte][a-la-carte] technique. The question then becomes, if we use the free monad should we raise all our monads into it, in the same way we do with monad transformers? So far I have not done this. My approach has been to put IO actions into the free monad but leave other monads (e.g. `Option` and `\/`) out of it. This means occasionally seeing nested `for` comprehensions but I find it simpler and more performant to work this way. One caveat: I haven't written a great deal of code using the free monad so my opinion might change.

[free-monad-interpreter]: {% post_url 2015-04-13-free-monads-are-simple %}
[free-monad-deriving]: {% post_url 2015-04-23-deriving-the-free-monad %}
[`Task`]: http://docs.typelevel.org/api/scalaz/nightly/#scalaz.concurrent.Task
[a-la-carte]: http://www.cs.ru.nl/~W.Swierstra/Publications/DataTypesALaCarte.pdf

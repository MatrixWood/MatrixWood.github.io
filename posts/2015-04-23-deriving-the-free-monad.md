---
layout: post
title:  "Deriving the Free Monad"
author: Noel Welsh
category: programming
repost: underscore
---

The free monad is defined by this structure[^defn]:

~~~ scala
sealed trait Free[F[_], A]
final case class Return[F[_], A](a: A) extends Free[F, A]
final case class Suspend[F[_], A](s: F[Free[F, A]]) extends Free[F, A]
~~~

We can use the free monad without understanding its implementation, but to *really* understand it we need to know why it is defined this way.

It certainly wasn't obvious to me why this is the correct definition, and reading the literature quickly devolved into "doughnoids in the category of pretzelmorphisms" land. Here I want to present an explanation aimed at programmers that doesn't involve abstract alphabet-soup. 
 
<!--more-->

## Preliminaries

The free monad represents the minimal possible structure to implement a monad and nothing else. This is achieved by separating the structure of monadic computations from the process that gives them meaning. The free monad gives us the means to construct a monad from any type (that is also a functor)[^coyoneda] by wrapping it in the free monad. 

Let me give a simple example of this separation of structure and meaning that doesn't involve any monads. Consider the expression

~~~ scala
1 + 2 + 3
~~~

When we write this expression we bundle the structure of the computation (two additions) with the meaning given to that computation (`Int` addition).

We could separate structure and meaning by representing the structure of the computation as data, perhaps as[^oops]

~~~ scala
Add(1, Add(2, 3))
~~~

Now we can write a simple interpreter to give meaning to this structure. Having separated the *abstract syntax tree* from the interpreter we can choose different interpretations for a given tree, such as computing with [dual numbers][dual-numbers] to automatically compute derivatives, or running the code on a GPU for performance.

The free monad is just an abstract syntax tree representation of a monad. It has the advantage that we can define custom interpreters for the computations represented in the free monad, and with some further tricks compose monads and interpreters[^a-la-carte].

We should be able to derive the free monad from the operations required to define a monad, as it is just a representation of those operations as a data structure. Before we dive into the free monad, however, I want to return to our example of addition and derive the free monoid. This serves as a useful warmup before we tackle the free monad.


## The Free Monoid

Our goal with implementing the free monoid is to represent computations like

~~~ scala
1 + 2 + 3
~~~

in a generic way without giving them any particular meaning.

The free monoid will wrap an arbitrary type and must itself be a monoid.

A monoid for some type `A` is defined by:

1. an operation `append` with type `(A, A) => A`; and
2. an element `zero` of type `A

The following laws must also hold:

1. `append` is associative, meaning `append(x, append(y, z)) == append(append(x, y), z)` for all `x`, `y`, and `z`, in `A`.
2. `zero` is an identity of `append`, meaning `append(a, zero) == append(zero, a) == a` for any `a` in `A`.

The monoid operations (`append` and `zero`) suggest we want a structure something like

~~~ scala
sealed trait FreeMonoid[+A]
final case object Zero extends FreeMonoid[Nothing]
final case class Append[A](l: A, r: A) extends FreeMonoid[A]
~~~

but this doesn't work -- we can't write, for instance, `Append(Zero, Zero)` because the types don't line up. 

We can use a structure like

~~~ scala
sealed trait FreeMonoid[+A]
final case object Zero extends FreeMonoid[Nothing]
final case class Value[A](a: A) extends FreeMonoid[A]
final case class Append[A](l: FreeMonoid[A], r: FreeMonoid[A]) extends FreeMonoid[A]
~~~

Now we can represent `1 + 2 + 3` as

~~~ scala
Append(Value(1), Append(Value(2), Value(3)))
~~~

This is not the simplest representation we can use. With a bit of algebraic manipulation, justified by the monoid laws, we can normalize any monoid expression into a form that allows for a simpler representation. Let's illustrate this via algebraic manipulation on `1 + 2 + 3`.

The identity law means we can insert the addition of zero in any part of the computation without changing the result, and likewise we can remove any zeros (unless the entire expression consists of just zero). We're going to decree that any normalized expression must have a single zero at the end of the expression like so: 

~~~ scala
1 + 2 + 3 + 0
~~~

The associativity law means we can place brackets wherever we want. We're going to decide to bracket expressions so traversing the expression from left to right goes from outermost to innermost bracket, like so:

~~~ scala
(1 + (2 + (3 + 0)))
~~~

With these changes -- which by the monoid laws make no difference to the meaning of the expression -- we can construct the following abstract syntax tree.

~~~ scala
sealed trait FreeMonoid[+A]
final case object Zero extends FreeMonoid[Nothing]
final case class Append[A](l: A, r: FreeMonoid[A]) extends FreeMonoid[A]
~~~

We can represent `1 + 2 + 3` (normalized to `(1 + (2 + (3 + 0)))`) as

~~~ scala
Append(1, Append(2, Append(3, Zero)))
~~~

The final step is to recognise that this structure is isomorphic (in the real, not the [Javascript][js-iso], sense) to `List`. So we could just as easily write

~~~ scala
1 :: 2 :: 3 :: Nil
~~~

or

~~~ scala
List(1, 2, 3)
~~~

Our final step is to make sure that `List` itself a monoid. It is. The monoid operations on `List` are:

- `append` is `++`, list concatentation;
- `zero` is `Nil`, the empty list; and
- we can "lift" any type into the free monoid using `List.apply`

High fives all around -- we've derived the free monoid from first principles.

## The Free Monad

We are now ready to tackle the free monad. We can take the same approach starting with the monad operations `point` and `flatMap`, but our task will be easier if we reformulate monads in terms of `point`, `map`, and `join`. Under this formulation a monad for a type `F[_]` has:

- an operation `point` with type `A => F[A]`;
- an operation `join` with type `F[F[A]] => F[A]`; and
- an operation `map` with type `(F[A], A => B) => F[B]`.

From this list of operations we can start to create an abstract syntax tree. We start with the definition of `Free`.

~~~ scala
sealed trait Free[F[_], A]
~~~

We can directly convert `point` into a case `Return` (following the names I introduced in the introduction).

~~~ scala
final case class Return[F[_], A](a: A) extends Free[F, A]
~~~

We are going to convert `join` into a case `Suspend`. What is the type of the value we store inside `Suspend`? We might think it should store a value of type `F[F[A]]` but if we did this we wouldn't be able to store, say, a `Return` inside the outer `F`. We can break it down like this:

- The inner `F[A]` will be represented by an instance of the free monad, and thus has type `Free[F, A]`.
- The outer `F[_]` will be wrapped in the `Suspend` we're creating.

Therefore the value we should store has type `F[Free[F, A]]` giving us

~~~ scala
final case class Suspend[F[_], A](f: F[Free[F, A]]) extends Free[F, A]
~~~

Finally we have `map`. This suggests a case like[^go-sub]

~~~ scala
final case class Map[F[_], A, B](fa: Free[F, A], f: A => B) extends Free[F, B]
~~~

This looks a little problematic. We have three type parameters while `Free` only has two. In fact we can do away with this case! We inherit `map` from monad being a functor. A `map` represents a pure, not an effectful, computation. We don't need to represent `Map` in the free monad abstract syntax tree so long as we can implement the `map` operation in our free monad.

Our final free monad data type looks like

~~~ scala
sealed trait Free[F[_], A]
final case class Return[F[_], A](a: A) extends Free[F, A]
final case class Suspend[F[_], A](s: F[Free[F, A]]) extends Free[F, A]
~~~

This is what we saw in the introduction. But does it really work? To show it does, let's implement the monad operations on this data type. We'll use the more familiar `flatMap` and `point` formulation, which is better suited to Scala, than the `point`, `join`, and `map` formulation above.

We can knock out `point` easily enough.

~~~ scala
object Free {
  def point[F[_]](a: A): Free[F, A] = Return[F, A](a)
}
~~~

Things get a bit trickier with `flatMap`, however. Since we know `Free` in an algebraic data type we can easily get the structural recursion skeleton.

~~~ scala
sealed trait Free[F[_], A] {
  def flatMap[B](f: A => Free[F, B]): Free[F, B] =
    this match {
      case Return(a)  => ???
      case Suspend(s) => ???
    }
}
~~~

The case for `Return` just requires us to follow the types.

~~~ scala
sealed trait Free[F[_], A] {
  def flatMap[B](f: A => Free[F, B]): Free[F, B] =
    this match {
      case Return(a)  => f(a)
      case Suspend(s) => ???
    }
}
~~~

The case for `Suspend` is a bit trickier. The value `s` has type `F[Free[F, A]]`. The only operation we (currently) have available is `f`, which accepts an `A`. We could `flatMap` `f` over the `Free[F, A]` wrapped in `F`, but we haven't yet required any operations on `F`. If we require `F` is a functor we can then `map` over it. Concretely, we can use this code snippet:

~~~ scala
s map (free => free flatMap f) 
~~~

A bit of algebra shows the result has type `F[Free[F, B]]`, and we can wrap that in a `Suspend` to get a result of type `Free[F, B]`. Our final implementation is thus

~~~ scala
sealed trait Free[F[_], A] {
  def flatMap[B](f: A => Free[F, B])(implicit functor: Functor[F]): Free[F, B] =
    this match {
      case Return(a)  => f(a)
      case Suspend(s) => Suspend(s map (_ flatMap f))
    }
}
~~~

We can write `map` in terms of `flatMap`

~~~ scala
def map[B](f: A => B)(implicit functor: Functor[F]): Free[F, B] =
  flatMap(a => Return(f(a)))
~~~

It's left as an exercise to the reader to prove the monad laws hold.

## Conclusions

In this blog post I've tried to explain how the free monad comes to be without invoking category theory. Hopefully this sheds a bit more light on the construction, and shows it's a natural consequence of the monad operations.

There is a lot more to the free monad than just constructing it -- using it is rather important too. I've linked to a few more ideas in the footnotes, but I have another blog post the describes why the free monad is interesting. Finally, our book [Essential Interpreters][advanced-scala] has complete coverage of the free monad (at least it will, when we've written it!)

[dual-numbers]: http://en.wikipedia.org/wiki/Dual_number
[js-iso]: http://isomorphic.net/
[advanced-scala]: /training/courses/advanced-scala-scalaz/

[^defn]: There are other ways of defining the free monad, but this is the most common in my reading.
[^oops]: This data structure can't actually be implemented. The right-hand element of `Add` is an `Add` in one case and an `Int` in another. We'll see how to actually implement this in the next section.
[^coyoneda]: The free monad requires that we wrap it around a functor. There is another trick, called the Coyoneda, that allows us to turn any type into a functor. This allows us to wrap any type with the free monad (by first constructing a Coyoneda functor for it). In this discussion we're not going to cover the Coyoneda so for our purposes the free monad can only be wrapped around a functor.
[^a-la-carte]: This extension is described in [Data Types a la Carte](http://www.cs.ru.nl/~W.Swierstra/Publications/DataTypesALaCarte.pdf) and eventually will be described in [Essential Interpreters](http://underscore.io/training/courses/advanced-scala-scalaz/).
[^go-sub]: If you look at the [Scalaz implementation](https://github.com/scalaz/scalaz/blob/series/7.2.x/core/src/main/scala/scalaz/Free.scala) of free monads you see a case very much like this called `GoSub`. This actually represents `flatMap` (read the types) but it isn't strictly necessary if we're not also implementing trampolining as Scalaz's implementation does.

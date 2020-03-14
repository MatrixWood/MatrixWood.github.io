---
layout: post
title: Annihilators in Scala
author: Noel Welsh
category: programming
repost: underscore
---

In this post I want to explore the design of a type class solving a problem that came up repeatedly in my current project. It's fairly general, so rather than diving into the details of the project, I'll start with a few simple examples:

- integer multiplication is *annihilated* by zero, in that once zero is introduced the result is always zero;
- set intersection is *annihilated* by the empty set, in that once the empty set is introduced the result is always the empty set; and
- field dereferencing using the "null-safe" `?.` operator in [Kotlin][kotlin-null-safe] and [Coffeescript][coffeescript-operators] is *annihilated* by `null`, in that once a `null` is introduced the result is always `null`.

<!--more-->

You can probably think of your own examples using boolean algebra, or floating point numbers, for example. I use the term *annihilated* as this is the same concept as an [annihilator from abstract algebra][annihilator], as I understand it. 

There are two parts to an annihilator:

- there is a binary operation; and
- there is an element that causes annihilation when using that operation.

From this we can define a type class.

~~~ scala
trait Annihilator[A] {
  def zero: A
  def product(a1: A, a2: A): A
}
~~~

I've used the names `zero` and `product` by analogy to multiplication, probably the best known instance. As it stands this interface is identical to a monoid, so we'd better add some laws to distinguish the semantics.

- *Annihilation:* `product(zero, a) = zero` and `product(a, zero) = zero`
- *Associativity:* although not strictly implied by the description above it seems a good idea to mandate that `product(product(a, b), c) == product(a, product(b, c))`
 
So far, so simple, but this is not really useful. Let me talk a bit about the example that motivated this blog post to illustrate why.

In our case we were dealing with various types of intervals. When we take the intersection of two non-overlapping intervals we end up with an empty interval, which annihilates all further intersections. Note that the empty interval has no start or end, unlike all non-empty intervals, so it requires a different representation. An algebraic data type like the below is representative[^invariant].

~~~ scala
sealed trait Interval[A]
final case class Nonempty(start: A, end: A) extends Interval[A]
final case object Empty[A]() extends Interval[A]
~~~

When writing methods on this type the following pattern comes up repeatedly.

~~~ scala
sealed trait Interval[A] {
  def intersect(that: Interval[A])(implicit order: Order[A]): Interval[A] =
    this match {
      case Empty() => Empty()
      case Nonempty(s1, e1) =>
      that match {
        case Empty() => Empty()
        case Nonempty(s2, e2) =>
          val start = order.max(s1, s2)
          val end = order.min(e1, e2)
          Nonempty(start, end)
      }
    }
}
final case class Nonempty(start: A, end: A) extends Interval[A]
final case class Empty[A] extends Interval[A]
~~~

The repeated nested pattern matching (aka structural recursion) gets fairly tedious, and got me looking for some way to eliminate it.

The first realisation is a type with an annihilator is isomorphic to an `Option`. We can divide the domain `A` into two subsets,

- the zero elements, which we map to `None`; and
- the non-zero elements, which we map to `Some`.

What type should the `Some` elements contain? Going back to our example we really want an `Option[Nonempty]`, not an `Option[Interval]`, or we'll have to do the pattern matching that we're trying to avoid. In general this means we must have a type for the non-zero elements, which we *refine* `A` to.

We can express this as

~~~ scala
trait Annihilator[A, R] {
  def zero: A
  def product(a1: A, a2: A): A

  def toOption(a: A): Option[R]
  def fromOption(o: Option[R]): A
}
~~~

where `R` is our refined type. 

With a suitable definition of `Annihilator` in scope, and some implicit class syntax, we can write

~~~ scala
sealed trait Interval[A] {
  def intersect(that: Interval[A])(implicit order: Order[A]): Interval[A] =
    (for {
      i1 <- this.toOption
      i2 <- that.toOption
    } yield {
      val start = order.max(i1.start, i2.start)
      val end = order.min(i1.end, i2.end)
      Nonempty(start, end)
    }).fromOption
}
final case class Nonempty(start: A, end: A) extends Interval[A]
final case class Empty[A]() extends Interval[A]
~~~

That's looking better. In this case we could simplify further by recognising that we only need an applicative, not a monad, but this tweaking is going off the path I want to explore.

Although this implementation is quite nice, it still feels a bit clunky. We're doing work to convert our `Interval` into an `Option`, only to immediately undo the conversion. It feels like there should be a simpler abstraction that avoids the conversion to `Option`.

We can merge the "conversion to `Option`" step with the "do something with the `Option`" to give us the following API.

~~~ scala
trait Annihilator[A, R] {
  def zero: A
  def product(a1: A, a2: A): A

  def refine[B](a: A)(then: R => B)(else: => B): B
  def unrefine(in: R): A

  def toOption(a: A): Option[R] =
    refine(a)(r => Some(r))(None)
  def fromOption(o: Option[R]): A =
    o.fold(zero)(unrefine _) 
}
~~~

Now we can write `intersect` directly in terms of `refine`.

~~~ scala
sealed trait Interval[A] {
  def intersect(that: Interval[A])(implicit order: Order[A]): Interval[A] =
    this.refine { i1 =>
      that.refine { i2 =>
        val start = order.max(i1.start, i2.start)
        val end = order.min(i1.end, i2.end)
        Nonempty(start, end)
      }(Empty())
    }(Empty())
}
final case class Nonempty(start: A, end: A) extends Interval[A]
final case class Empty[A]() extends Interval[A]
~~~

This feels like the right abstraction to me. We should also consider some laws for these new operations. It seems natural to me that an identity law should hold.

*Identity:* `a == refine(a)(r => unrefine(r))(a)`

We can define some derived operations to make further simplifications, if we so desire.

~~~ scala
trait Annihilator[A, R] {
  def zero: A
  def product(a1: A, a2: A): A

  def refine[B](a: A)(then: R => B)(else: => B): B
  def unrefine(in: R): A

  def and(a1: A, a2: A)(then: (R, R) => B)(else: => B): B =
    refine(a1){ r1 =>
      refine(a2){ r2 =>
        then(r1, r2)
      }{
        else
      }
    }{
     else
    }

  def toOption(a: A): Option[R] =
    refine(a)(r => Some(r))(None)
  def fromOption(o: Option[R]): A =
    o.fold(zero)(unrefine _) 
}
~~~

With `and` we can define a more compact version of `intersect`.

~~~ scala
sealed trait Interval[A] {
  def intersect(that: Interval[A])(implicit order: Order[A]): Interval[A] =
    (this and that){ (i1, i2) =>
      val start = order.max(i1.start, i2.start)
      val end = order.min(i1.end, i2.end)
      Nonempty(start, end)
    }{
      Empty()
    }
}
final case class Nonempty(start: A, end: A) extends Interval[A]
final case class Empty[A]() extends Interval[A]
~~~

The `and` method is equivalent to a "collapsed" applicative operation in the same way `refine` is a collapsed monad operation. In fact we can view `Annhilator` as an `Option` without the higher-kinded structure, with equivalent monadic and applicative operations that have been "lowered" from `F[_]` to `F`. I don't know of any other work that has studied these objects --- if you know of any please mention it in the comments.

Our structure is closely related to [refinement types][refinement-types] (at least as I understand them). A refinement type is a type along with a logical predicate that narrows the domain of the type. The `refine` operation merges the predicate with doing something based on the predicate result. This merging is, I think, necessary to represent the actual type refinement in Scala's type system. The refinement operations could well be extracted from `Annihilator` and made into their own separate type class.

In the current design there is only a single `zero`. I briefly considered an alternate design that would allow multiple zeros, modelling them as a predicate `isZero`. This design is closer to refinement types, and is a better fit for, say, IEEE floating point numbers that have multiple representations of NaN, the natural zero on that type. It would be interesting to explore this design further.

Finally, if you want to explore the design I have made [the code][code] available.

[^invariant]: I've made `Interval` invariant because, in general, covariance doesn't play nicely with type classes. This is worth another blog post of its own.

[kotlin-null-safe]: http://kotlinlang.org/docs/reference/null-safety.html
[coffeescript-operators]: http://coffeescript.org/#operators
[annihilator]: https://en.wikipedia.org/wiki/Annihilator_(ring_theory)
[refinement-types]: http://goto.ucsd.edu/~rjhala/liquid/haskell/blog/blog/2013/01/01/refinement-types-101.lhs/
[code]: https://github.com/underscoreio/annihilator

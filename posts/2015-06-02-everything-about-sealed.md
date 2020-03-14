---
layout: post
title: Everything You Ever Wanted to Know About Sealed Traits in Scala
category: programming
repost: underscore
---

Sealed traits are essential for idiomatic Scala code, but many developers are hazy on the details of their working. In this post we describe why you want to use them, and how to use them correctly to increase the quality of your code.

<!--more-->

# Algebraic Data Types

We can explain sealed in terms of its low-level semantics, but I find it much more useful to start with the big picture. The most important use of sealed is in defining *algebraic data types*. Despite their fancy name, algebraic data types are just a way of modelling data in terms of two patterns:

- *logical ors*, such as [`List`][list], which is a `::` or `Nil`; and
- *logical ands*, such as [`::`][double-colon], which has a `head` and a `tail`.

In functional programming lingo we call the logical or a *sum type* and the logical and a *product type*.

Algebraic data types are really really important. The standard library is chock full of them (`Option`, `List`, and `Try` are some examples) and you can probably think of places you can use them in your own work. A common example is modelling different types of users. You might have a structure like an `Account` that can be an `Admin` or a normal `User`. An `Account` will have certain properties like an `emailAddress`, a `username`, and so on. `Admins` probably have properties unique to them such as an `accessLevel`. This complex definition is just an algebraic data type --- it is defined entirely in terms of ands and ors.

Using some simple patterns we can mechanically translate the description of an algebraic data type into code. I won't go into the detailed Scala implementation of the sum and product type patterns here, but let's see a quick example for `List` described above[^full-pattern]:

~~~ scala
sealed trait List[+A] {
  // lotsa methods in here ...
}
final case class ::[A](head: A, tl: List[A]) extends List[A]
final case class Nil extends List[Nothing]
~~~

Notice the use of `sealed` (and `final`).

## Structural Recursion

So, algebraic data types are really useful and we define them using `sealed` traits, but what does sealing a trait actually get us? It's time to look at how we write code that uses algebraic data types. This also has a fancy name, *structural recursion*, but the basic idea is simple.

Let's start with a very familiar example, `Option`. `Option` is a `Some` or `None` (a sum type), and `Some` has an element `x` (a product type, albeit a very simple one containing a single field). How do we write a `match` expression for `Option`? We need:

- one case for `Some` and one for `None`; and
- the case for `Some` should do something with the element `x`.

In code:

~~~ scala
anOption match {
  case None    => doNoneCase
  case Some(x) => doSomeCase(x)
}
~~~

We can generalise this pattern to any algebraic data type. Each branch in a sum type (a logical or) gets its own `case` in the pattern matching, and each product type (a logical and) requires us to extract and do something with the elements. This is all that structural recursion is.

In the same way that we can mechanically convert the description of an algebraic data type into code, we can mechanically convert an algebraic data type into a skeleton for using that type.

Now we've seen three neat things: we can model a very general class of data using algebraic data types, we can mechanically convert an algebraic data type into Scala definitions, and we can mechanically write Scala code to use any algebraic data type via structural recursion. But we still haven't said what `sealed` gets us. Let me seal the deal and finally explain their importance.

## Exhaustiveness Checking

When we define an algebraic data type using `sealed` traits we allow the compiler to perform exhaustiveness checking. In simpler words, this means the compiler will shout at us if we miss out a case in our structural recursion.

Here's an example at the Scala console:

~~~ scala
scala> Option(1) match {
     |   case None => "Yeah"
     | }
<console>:8: warning: match may not be exhaustive.
It would fail on the following input: Some(_)
              Option(1) match {
                    ^
~~~

Exhaustiveness checking is extremely useful. You may have used Java APIs that sometimes return `null`, or perhaps Ruby which uses `nil` where in Scala we'd use an `Option`. If so you'll know how easy it is to forget to check for the `null` (or `nil`) case. Exhaustiveness checking completely prevents this type of error. It is also extremely useful when refactoring code. For example, if we extend an algebraic data type (say we add a new type of `User`) the compiler will tell us every place in our code base that needs to be updated.

Exhaustiveness checking allows us to make stronger guarantees about the correctness of our code, but only if we write our code in a way that lets the compiler do these checks for us. This is where `sealed` comes in.

## The Inner Life of Sealed

A `sealed` trait can only be extended within the file in which it defined. This allows the compiler to perform exhaustiveness checking for pattern matches on that trait. Why? Because the compiler must know all the possible subtypes to do the checks, and given that the JVM allows code to be loaded at runtime the compiler can't scan the whole program to collect all the possible subtypes (and even if it could it would prevent separate compilation and be rather slow).

A small example illustrates this.

First we declare a sealed trait and some subtypes:

~~~ scala
sealed trait Base
final case class SubtypeOne(a: Int) extends Base
final case class SubtypeTwo(b: Option[String]) extends Base
~~~

Now we can see that the compiler provides exhaustiveness checking. We declare an instance with type `Base` and we get a warning on an incomplete pattern match as we expect:

~~~ scala
scala> (SubtypeOne(1) : Base) match {
     |   case SubtypeOne(a) => a + 2
     | }
<console>:11: warning: match may not be exhaustive.
It would fail on the following input: SubtypeTwo(_)
              (SubtypeOne(1) : Base) match {
                             ^
~~~

(It's a good idea to turn warnings into errors with the `-Xfatal-warnings` compiler flag.)

Now on to the point that a lot of Scala programmers are hazy on: `sealed` is *not* transitive. Meaning that although `Base` above is `sealed`, this does not mean that `SubtypeOne` and `SubtypeTwo` are sealed and hence we do not get exhaustiveness checking when we match on a value with type `SubtypeOne` or `SubtypeTwo`. For example we get no warning that we're missing the `None` case here, instead getting a `MatchError` exception at runtime.

~~~ scala
scala> SubtypeTwo(Some("oops")) match {
     |   case SubtypeTwo(None) => "Yeah!"
     | }
scala.MatchError: SubtypeTwo(Some(oops)) (of class SubtypeTwo)
~~~

Let's be clear on what's going on here:

- we are matching on a value with type `SubtypeTwo` not type `Base`;
- `SubtypeTwo` is not `sealed`;
- therefore the compiler cannot guarantee it knows everything about `SubtypeTwo` (there could be subtypes defined in another file); and
- thus we do not get exhaustiveness checking.

You might argue that the compiler should give us a warning about the `None` case here, since `Option` is `sealed`, but doing so would give unpredictable behaviour in general -- we would sometimes get the checking and sometimes not depending on how exactly we defined our types and matches. It's much better to have predictable semantics than to build a more complicated system that leads to surprises (and unexpected runtime crashes)!

Note that we can get exhaustive checking in the example above if we declare the type as `Base`. Exhaustiveness checking is controlled entirely by the type of the expression being matched.

~~~ scala
scala> (SubtypeTwo(Some("oops")) : Base) match {
     |   case SubtypeTwo(None) => "Yeah!"
     | }
<console>:11: warning: match may not be exhaustive.
It would fail on the following inputs: SubtypeOne(_), SubtypeTwo(Some(_))
              (SubtypeTwo(Some("oops")) : Base) match {
                                        ^
~~~

## Final Words

The `final` modifier has similar semantics to `sealed`. A sealed trait can only be extended in the defining file, while a final class cannot be extended anywhere. In what seems to me an odd quirk, final classes do *not* get exhaustiveness checking.

Despite this I make case classes in algebraic data types `final`, as in the examples above, and as in the standard library (see [Some][some] for example). There are few reasons for this. Firstly, I find it is more descriptive. It also opens up more optimisation possibilities than `sealed` (though I don't know if these opportunities are taken advantage of). Finally, when declaring types we almost always use the base type (e.g. `Option`) instead of a subtype (e.g. `None`) so the lack of exhaustiveness checking is very rarely an issue.

If you look at the standard library you'll see sealed abstract classes are often used where I've used sealed traits in the examples here. I believe sealed abstract classes lead to a slightly faster implementation and easier Java interoperation. In my own practice I like to minimise the number of concepts I use, and as traits are generally more useful than abstract classes I prefer them.

*Update* If you made it this far you should check out the [followup] post.

[double-colon]: http://www.scala-lang.org/api/current/index.html#scala.collection.immutable.$colon$colon
[list]: http://www.scala-lang.org/api/current/index.html#scala.collection.immutable.List
[some]: http://www.scala-lang.org/api/current/index.html#scala.Some
[followup]: {% post_url 2015-06-04-more-on-sealed %}

[^full-pattern]: In addition to illustrating sum and product types, this example also contains covariance. I decided it was better to use a more realistic example in this blog post, rather than an abstract definition showing just sum and product types.

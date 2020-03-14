---
title: Type Classes vs Records of Functions
---

Type classes and records of functions are two very similar tools that are available in languages like Haskell, Scala, and Rust. Given the similarity the question arises of which we should use. In this blog post I discuss when I think one language mechanism should be preferred over the other.

<!--more-->

## A Who of What?

Before we get into the details, lets just clear up the terminology. I'm going to assume you know what a type class is. A record is the Haskell term for what in Scala we'd call a `class` (or `trait`, or `case class`) and in Rust a `struct`. Basically it's a data structure where the fields (the bits that store the data) have names. So a record of functions is a data structure with named fields where the fields hold functions. In OO parlance fields that hold functions are methods, so we're basically talking about objects.

An object, or record of functions, is very similar to a type class. In fact if we forget about the compile-time dispatch that type classes bring it's the same thing. This similarity is why the question arises as to which we should use.


## Composition

Type class composition is one nice feature that records of functions don't provide. The basic idea is that if we define the base elements and the composition rules, the compiler will automatically apply the composition rules to construct a type class instance.

For example, if we have a `Monoid` type class such as

```scala
trait Monoid[A] {
  def combine(x: A, y: A): A
  def identity: A
}
```

and instances

```scala
implicit object intMonoid extends Monoid[Int] {
  def combine(x: Int, y: Int): Int =
    x + y

  val identity: Int = 0
}

implicit def optionMonoid[A](implicit m: Monoid[A]): Monoid[Option[A]] =
  new Monoid[Option[A]] {
    def combine(x: Option[A], y: Option[A]): Option[A] =
      (x, y) match {
        case (Some(x), Some(y)) => Some(m.combine(x, y))
        case (Some(x), None)    => Some(x)
        case (None,    Some(y)) => Some(y)
        case (None,    None)    => None
      }

    val identity: Option[A] = None
  }
```

we don't have to explicitly define `Monoid[Option[Int]]`. The compiler will construct the instance for us. No such automatic composition takes place with records of functions.[^extensible-records]

So here is our first criteria for deciding if we should use type classes or records of functions: will type class composition bring us a lot of value? If not, perhaps we shouldn't use a type class.

[^extensible-records]: There are some languages and libraries that allow adding and removing fields from records at runtime. They are generally known as "extensible records". To the best of my knowledge no such system is in widespread use. OO languages allow compile-time composition (usually called extension or inheritance). Neither system allows automatic type-driven composition, which is the magic that type classes bring.


## Comprehensibility

You'll sometimes see functional programmers ranting about "lawless type classes". This combines two of (some) functional programmers favourite things: impenetrable jargon and getting bent out of shape. However, there is some point to this discussion.
Type class "laws" are statement, usually expressed in maths, of some properties that must hold for a type class instance to be considered valid. For example, for `Monoid` the laws are:

* the binary operation must be associative, so `combine(x, combine(y, z)) == combine(combine(x, y), z)`; and
* the identity must be a commutative ... umm ... identity, so `combine(a, identity) == a == combine(identity, a)`.

Defining the semantics is important because it's these semantics that allow us to implement systems using type classes and know they'll work as we expect. Let's address this from two different angles.

One way of seeing the importance of clearly defined semantics is to consider how many possible type class instances we could implement for `Monoid` if we disregarded the laws. Almost anything would be valid. For example, we could define

```scala
implicit object intMonoid extends Monoid[Int] {
  def combine(x: Int, y: Int): Int =
    x * (y - 42)

  val identity: Int = 42
}
```

or indeed any other `combine` and `identity` that match the type signature. If this was common place how would we ever understand what code using type classes will do? Type classes seem fairly magical in the first place. We need laws to allow us to build understanding when they are being used.

As another example, monoids are really [useful][algebird] in [data science][monoidify] because they allow us to easily parallelize jobs. Remember associativity says that `a + (b + c) == (a + b) + c`. If we consider a pair of brackets to be a machine, if the operation we want to perform is a monoid it doesn't matter how we distribute work across those machines so long as retain the ordering of the data (and if the operation is commutative, which most are, the order doesn't matter at all). Here the clearly defined semantics allow to guarantee that our distributed system will be correct.

Some type classes don't have laws. This is what a "lawless type class" is (sadly, not some lovable functional programming rogue that steals from the rich to give to the poor.)

Now, are lawless type classes bad? Not necessarily! Lawlessness (gosh, I hate this pompous terminology) is a proxy for the real issue, which is program understanding. If a type class has laws then you have some principles you can use to reason about your program. A lawless type class might still have principles you can use to reason about code. For example, the 0.9 branch of [Doodle][doodle] has a `Renderer` type class. Its role is to render (i.e. draw) a picture to some kind of canvas. There aren't any useful laws you can write about this without introducing a huge amount of mathematical machinery to describe the effect of rendering. However it's still fairly easy to understand what this type class does.

So here is our second criteria: if we can clearly define the semantics of an interface it could be a type class. If the interface is just a blob of functions that "do stuff", like we might find inside the implementation of an interpreter, it probably should not be a type class.


## Multiple Instances

Type classes use types to choose type class instances. In other words they dispatch on type. Different languages have different rules for where they find type class instances, but in all cases that I know of the compiler will search outside the usual lexical scope for instances. If there are multiple instances for the same type it could be difficult to work out which one is used. Haskell forbids this. Scala allows multiple instances but if two or more are in scope code will not compile. I'm not sure what the situation is with Rust.

Where multiple instances are allowed there are clear rules for which instance is being used, but that doesn't mean it's easy for the programmer to work out what the compiler will do. For example, the choice of instance might depend on imports that are far away from the code being viewed, or on complicated priority rules.

This gives us another criteria for choosing between type classes and records of functions: if we could want multiple instances for the same type we're probably better using records of functions than type classes. This is really another facet of "Comprehensibility" above. Using multiple instances can lead to confusion when we're relying on the compiler to choose instances for us.


## Convenience

The final issue I want to consider is convenience. A library's API is the user interface through with the programmer interacts with it, and using type classes can make that API easier to use. For example, in Doodle making the `Renderer` a type class means the user doesn't have to explicitly specify the `Renderer` when calling the `draw` method on a picture. This makes drawing easier. The user needs to know less about the library to get something working.

So my final criteria for choosing between type classes and records of functions: will using type classes make the code easier to use for the end user?


## Summary

We've seen a few different reasons for choosing between type classes and records of functions. In summary they are:

* if automatic composition will be useful then consider type classes;
* if semantics are not well defined don't use a type class;
* if multiple instances are likely a type class is probably the wrong choice; and
* if it will make your code substantially easier to use a type class might be the right choice.

Hopefully this will make the decision easier for you.


[algebird]: https://github.com/twitter/algebird
[monoidify]: https://arxiv.org/abs/1304.7544
[doodle]: https://www.creativescala.org/doodle/

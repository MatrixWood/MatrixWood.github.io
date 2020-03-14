---
layout: post
title: More on Sealed Traits in Scala
category: programming
repost: underscore
---

My [recent post][sealed-post] on sealed traits had some great feedback but it was clear that I glossed over some points too quickly. In this post I want to go over a new trick I've learned for sealed traits and clear up some of the points I made in the last post.

<!--more-->

## The Product with Serializable Trick

If you declare algebraic data types with the pattern I gave in the last post you might see the compiler inferring types that unexpectedly include `Product` and `Serializable`. For example if you declare

~~~ scala
sealed trait Color
final case object Red extends Color
final case object Green extends Color
final case object Blue extends Color
~~~

when you put elements into a list the compiler will infer the elements have type `Product with Serializable with Color`.

~~~ scala
scala> val colors = List(Red, Green, Blue)
colors: List[Product with Serializable with Color] = List(Red, Green, Blue)
~~~

This happens because all case classes automatically extend `Product` and `Serializable`. To avoid this ugly type we can make our base type (the sealed trait) extend these two types as well.

~~~ scala
sealed trait Color extends Product with Serializable
final case object Red extends Color
final case object Green extends Color
final case object Blue extends Color
~~~

Now we don't see `Product` and `Serializable` in the inferred type.

~~~
val colors = List(Red, Green, Blue)
colors: List[Color] = List(Red, Green, Blue)
~~~

Thanks to Julian Truffaut and Channing Walton for pointing this out to me.

## Why You Want to Use Final

I suggested you should mark case classes in algebraic data types as `final`, but didn't explain very clearly why.

A final case class cannot be extended by any other class. This means you can make stronger guarantees about how your code behaves. You know that nobody can subclass your class, override some methods, and make something goofy happen. This is great when you are debugging code -- you don't have to go hunting all over the object hierarchy to work out which methods are actually being called.

Of course making classes final does mean that you lose a form of extensibility. If you do find yourself wanting to allow users to implement functionality you should wrap that functionality up in a trait and use the type class pattern instead.

## Base Types vs Sub-types

In the previous post I rather cryptically said

<blockquote>
Finally, when declaring types we almost always use the base type (e.g. `Option`) instead of a subtype (e.g. `None`) so the lack of exhaustiveness checking is very rarely an issue.
</blockquote>

Say you define an algebraic data type

~~~
sealed trait Base
final case class Foo(a: Int) extends Base
final case class Bar(a: String) extends Base
~~~

You should almost always just refer to the `Base` trait when you declare types, not the subtypes. Some good reasons:

- to ensure you get exhaustiveness checking;
- to avoid issues with invariant type classes and invariant containers like `Future`; and
- it's often just doesn't make sense to declare a subtype (why would a method ever declare return type as `None`?).

As I explained in the previous article, only sealed types turn on exhaustiveness checking. In the example over this means that values with type `Base` will have exhaustiveness checking but values of type `Foo` and `Bar` will not. That's one good reason to declare method, return, or variable types as `Base`.

Type classes are one of the other big patterns I advocate for Scala. For reasons I won't go into here (look out for a future blog post) invariant type classes generally work best in Scala. In an invariant type class means that `TypeClass[Foo]` is *not* a subtype of `TypeClass[Base]` and this will cause annoyance. Just use `Base` everywhere and the problem goes away.

You might have already run into this issue if you use invariant containers such as `Future`.

Finally, there often isn't a lot of sense in declaring something to have a type that is not the base type of an algebraic data type. For example, if a method were to only return `None` it should just return `()`. If it was to return only `Some` it should just return the value wrapped in the `Some`.

[sealed-post]: {% post_url 2015-06-02-everything-about-sealed %}

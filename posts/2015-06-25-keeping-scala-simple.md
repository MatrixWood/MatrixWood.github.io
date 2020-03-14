---
layout: post
title: Keeping Scala Simple
author: Noel Welsh
category: programming
repost: underscore
---

You don't have to venture far to find people arguing that Scala is a complex language, or that Scala needs to be more opinionated. Luckily I have plenty of opinions, specifically about how to make Scala simpler, and this is something I've been espousing in my recent talks at Scala Days SF and Amsterdam ([slides here][slides]). 

The problem with talking about simplicity is that it becomes one of those things like "good art" that's defined as "I know it when I see it." This provides no guidance. We need more precision. In this blog post I want to discuss complexity from three different angles and give concrete recommendations for creating simple Scala code.

<!--more-->

## Syntactic Simplicity

Scala's syntax is a bit more flexible than many people are used to, and the combination of rules can lead to some surprises. For example

~~~ scala
Set.empty()
~~~

expands to

~~~ scala
Set.empty.apply()
~~~

due to a slightly surprising interaction between methods with no arguments and the shorthand for `apply`. (This syntax is actually deprecated and the compiler will issue a warning if you use it.)

Right-associative operators (e.g. `1 +: Seq(2, 3)`) are a good example that I don't think pay their way in terms terms of complexity. They add some conceptual overhead for what is a very infrequently used feature. Overall, though, I don't find Scala's syntax difficult to teach so I don't have any serious complaints here.

I do have issues with the use of symbolic operators in Scala libraries. The collections library shortcut for folds (`:\` and `/:`), for example, should be removed in my considered opinion. These operators provide no utility over `foldLeft` and `foldRight`, and only serve to increase the mental load on the reader. Many other libraries have similarly come to regret heavy use of symbolic operators (recall the infamous [dispatch periodic table][periodic-table]). Luckily we are seeing declining use of symbols in Scala libraries, and I think the community as a whole has sufficient experience that we can state:

*Simple code does not use symbolic operators unless:*

- *there are only a few such operators in use; and*
- *they are used frequently enough that they are worth the mental load to remember what the symbols mean.*

A good example is the applicative builder syntax of `|@|` in Scalaz and Cats. It is typically the only syntax used with applicatives, and it comes up in enough cases that it is worth remembering.

## Semantic Simplicity

Syntax is only the surface covering of the language, and we must look deeper -- at the semantics -- to find another source of complexity. Adding features adds new semantics, but it is easy to use basic features to create programs that are tricky to understand. Imagine the following code *exists within a larger program* and ask yourself what the value of `Example.foo(1)` is.

~~~ scala
object Example {
  var x = 0

  def foo(y: Int): Int =
    if(x < 0)
      y - x
    else if(x == 0)
      y / 2
    else
      y + x
}
~~~

You can't answer the question without inspecting the larger program. The reason being that the meaning of `Example.foo` changes depending on the value of `x`, and `x` can be changed at any time by any other code.

With semantics we have a formal system to study, which gives use precise words to explain concepts. In this case the concept is *local reasoning*. A program enables local reasoning if we can work out the meaning of any part by looking only at that part. It is closely related to the property of *substitution*, which means we can replace any expression by its value without changing the meaning of a program. Any program that maintains substitution enables local reasoning, but the converse is not true. For example

~~~ scala
def sum(xs: Seq[Int]): Int = {
  var total = 0
  xs foreach { x =>
    total = total + x
  }
  total
}
~~~

does not strictly main substitution (the use of a mutable variable breaks substitution within `sum`) but nonetheless maintains local reasoning as `total` is not visible outside `sum`.

*Simple code maintains local reasoning wherever feasible.*

## Conceptual Simplicity

Although Scala has many features I don't find the semantics of the individual features taxing. Complexity comes in their interactions. Humans are pattern recognition machines, and ultimately simple code means reusing a small set of patterns across the code base. In other words, simplicity is being opionionated in how language features are used and combined. Conceptual complexity is, in my opinion, the most important form of complexity to control.

It's this final form of complexity that I haven't seen discussed much in the Scala community and it's where I think we as a community need to focus our attention. My contention is that *95% of your code should use four concepts of:*

- *algebraic datatypes;*
- *structural recursion;*
- *sequencing functions (primariliy `map`, `flatMap`, and `fold`); and*
- *type classes*.

Notice that these concepts involve the combination of many features. For more detail on these see [my slides][slides].

In the six or so years I've been using Scala this is what my programs have evolved to, and I find this code to be very maintainble. It's also code that inexperienced developers can easily work with. We train people in these concepts in [Essential Scala][essential-scala], our introductory Scala course, and have mentored many new teams in their use.

## Conclusions

Scala can be simple if you're prepared to be opinionated. In my opinion simplicity comes down to using algebraic datatypes, structural recursion, sequencing combinators, and type classes. I'm pleased to see Dotty, the next version of Scala, focusing on language simplicity. I think we, as the Scala community, need to have a larger discussion about how we use the language features we have. Other languages like Python have benefited from developing an accepted community style. I believe Scala can shed its image as a complex language, leads to a stronger and more cohesive community, if we do the same.

[slides]: http://noelwelsh.com/downloads/scala-days-amsterdam-2015.pdf
[periodic-table]: http://www.flotsam.nl/dispatch-periodic-table.html
[essential-scala]: http://underscore.io/training/courses/essential-scala/

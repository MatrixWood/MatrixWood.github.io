---
layout: post
title: Learn Shapeless, Find $500
category: programming
repost: underscore
---

I spent a few hours learning [Shapeless](https://github.com/milessabin/shapeless) and it made me $500.

At ScalaDays 2014, [Originate](http://www.originate.com/) ran a [competition](http://www.originate.com/stories/scala-days-2014) to implement a simple stack-oriented (aka concatenative) language. The competition hit all three points on my "interesting competition" scoring metric:

- the subject matter is intrinsically interesting to me;
- I thought I could implement a basic solution quite quickly; and
- there were clear avenues to expand the solution if I found time.

A bit of background. A [stack-oriented language](http://en.wikipedia.org/wiki/Stack-oriented_programming_language), as I understand them, operates by passing parameters on a stack. If you want to add two numbers you push them onto the stack, and them push on the `add` operation -- which in turn pops off the two numbers, adds them, and pushes the result back onto the stack. It's a very simple model, which has made it popular for some embedded systems, though I have my doubts about how it scales with program size.

The core of the competition is to implement a statically typed concatenative language, which boils down to implementing a heterogenously typed stack. A normal `List` won't do, because if we store, say, an `Int` and `String` in such a list we'll end up with a `List[Any]`. What we want is to the store the type of each element separately. This abstraction is called an `HList` and is one of the core features of [Shapeless](https://github.com/milessabin/shapeless). I was fairly sure I could use Shapeless to get my basic implementation done, and I was right.

Some of the code was quite straightforward using Shapeless. For example, pushing an element onto the stack is just `a :: stack` as you'd expect. However I had some difficulty typing binary operations. For these I needed to ensure the stack had at least two elements, and those elements had the expected type. To do this I needed to dig a bit deeper into how Shapeless is implemented.

The key insight is that Shapeless uses implicits to provide evidence that the `HList` has the type we're interested in. It supplies a number of implicits for common operations, but as far as I can tell no implementation extracting the first two elements of a list. In a few hours, and after some quality time with the Shapeless source code, I managed to implement my own `IsBinary` evidence. The actual construction is rather involved, with a healthy amount of indirection and even a method dependent type. But hey, that's how we roll. To be honest I was just copying the patterns from the Shapeless code.

Rather than trying to explain the code it's simpler if I just [link to it](https://github.com/noelwelsh/concatenative-lang) and you can digest at your leisure. The entire code base is only 112 lines.

All up this was a fun contest. I was very happy (and surprised!) to win the second place entry, though really it is all credit to Shapeless.

---
layout: post
title: "Why I Don't Like Akka Actors"
description: "The Three Deadly Sins of Actors"
category: programming
tags: [punditry, jvm]
lead: Akka's actor framework very popular in the Scala community, but I think they are the wrong model for managing concurrency in Scala. Here are three reasons why I don't think Akka has got it right.
---
{% include JB/setup %}

We recently rewrote [Myna's](http://www.mynaweb.com/) back-end service. The architecture changed dramatically, and is now both faster and easier to extend. One of the significant architectural changes was removing all [Akka](http://akka.io) [actors](http://en.wikipedia.org/wiki/Actor_model). After heavily using them in the first version of the back-end, I have come to prefer other methods of managing concurrency. Since Akka's actors are so prominent within the Scala community I thought it might be of interest to describe why we made this change.


## Actors are Coarse Abstractions

Actors are presented as a universal primitive for concurrency. That is, in the orthodox actor world view they are all you need for any concurrent program. There is an appealing conceptual simplicity to this approach, and the idea of finding an uber-abstraction has been successful in other contexts. For example, Scala's unification of Java's primitive and object types, or Python representing all values as mutable dictionaries as generally considered positive points by their respective language communities.

Problems arise when in the quest for simplicity important distinctions are hidden. In programming languages this usually comes up when discussing performance. The distinction between primitive and object types really matters if you care about speed. Scala gets away with this for the most part through clever compilation in both the Scala compiler and Hotspot, but writing high performance code can still be something of a dark art[^Python].

[^Python]: Python basically does nothing about this issue, which is why it's so slow. In fact the decision to make everything a mutable dictionary is one big reason it's so hard to optimise Python. [PyPy](http://www.pypy.org/), a JIT compiler for Python, has consumed 10 years and several million currency units and is still not widely deployed.

Concurrent programming involves at least three distinct concerns: concurrency, mutual exclusion, and synchronisation. With actors the first two always come combined, and you're left to hand-roll your own synchronisation through custom message protocols. It's an unhappy state of affairs if you want to do something as simple as separating control of concurrency and mutual exclusion. This is not an esoteric concern -- it is exactly what a [ConcurrentHashMap](http://docs.oracle.com/javase/7/docs/api/java/util/concurrent/ConcurrentHashMap.html) provides, for example. If you're really seeking performance then you probably want to use [lock-free algorithms](http://en.wikipedia.org/wiki/Non-blocking_algorithm). Again, these don't fit into the actor model. Basically the actor model is forcing us to give up a lot of tools so we can fit within its rigid conception of a concurrent program.


## Actors do not Compose

Composition is a desireable property of abstractions. Functions compose. If I create some functions (say, plus and minus) you can create another function (say multiply) that uses my functions. In particular I don't have to anticipate your usage ahead of time to allow you to use my functions.

Actors don't compose. By default actors hard-code the receiver of any messages they send. If I create an actor A that sends a message to actor B, and you want to change the receiver to actor C you are basically out of luck. If you're lucky I anticipated this in advance and made it configurable, but more likely you have to change the source. Lack of composition makes it difficult to create big systems out of small ones.


## Akka's Actors are not Usefully Typed

Akka's actors give you static typing within a single actor, but the communication between actors -- the complex bits that are most likely to go wrong -- are not typed in any useful manner. I could live with the above two issues, but this one really gets me.

The type system is the reason we use Scala. Types allow us to guarantee certain properties of our programs. If you've never used a modern statically typed programming language you might be surprised just how far you can push this. We try to push it reasonably far, so we can guarantee that, for example, Myna's API generates useful error messages (this is important because the API is the UI for many users). In return for this awesome power we put up with a bit of extra complexity compared to a dynamically typed language.

Akka supports a number of features, such as [become](http://nurkiewicz.blogspot.co.uk/2012/11/becomeunbecome-discovering-akka.html) and transparent distribution, that make statically typing messages difficult. We still have some inconvenience over dynamically typed languages but we lose the benefits of static typing. This is the wrong tradeoff for me.

Other languages, like Concurrent ML and Haskell, have demonstrated it's possible to have great concurrent and distributed programming abstractions in a statically typed language. I expect the same in Scala.


## So What Does Myna Use?

So given the above, what does Myna use? We use Akka's Futures, which I think are fantastic. We use plain-old locks for some simple cases where we want mutual exclusion, and we use a few of the utilities in `java.util.concurrent`. It's quite simple and it's quite fast: 2.5ms average response time, and well over 650 requests/s on a single core machine.

---
layout: post
title: "Expressive Width in Programming Languages"
description: ""
category: programming
lead: Programmers often talk about expressivity, but this doesn't capture a lot of what is useful about a programming language. Real programs run in a specific implementation, and implementations differ in how much control they give you over the machine. How well do languages balance the two?
---
{% include JB/setup %}

As programmers we often talk about the expressivity of a language, which is usually taken to be something like how concisely a large class of programs can be written. This neglects the important fact that we program to real implementations on real computers. Implementations exist to allow us to control the machine, and they differ greatly in how much control they give the programmer. I think it's more useful to talk about the combination of expressivity and control provided by a particular language and implementation, which I term "expressive width".

Languages like Scala and Racket are expressively fat. You can write compact code in them, but you can also write high performance code, and fiddle with things like [CAS](http://en.wikipedia.org/wiki/Compare-and-swap) operations or futures if multi-core concurrency and parallelism is the goal. Furthermore they both have a nice set of libraries, though obviously Scala has a far wider range available, thanks to the JVM. All up, there are a broad class of problems that you can effectively tackle with each.

Languages like Ruby and C are expressively thin. Ruby code is quite compact, and it has quite a wide range of libraries, but it's far too slow for anything performance sensitive. C is the opposite -- it's fast and you can do anything the OS provides -- but it's at such a low level of abstraction that it's not worth using for anything that isn't performance critical.

I greatly prefer expressively fat languages. They can be applied to a wider range of problems, by definition. There is a great deal to be gained by sticking to one language. For a start it makes all your code equally hackable. You don't end up with systems that only a one programmer in the organisation knows how to modify. It also simplifies the build system, and you can avoid dealing with the FFI. Finally it's much easier to find other people skilled in one language than those skilled in two or more.

Some argue that one should pick the tool for the job, but I don't see that happening very often in practice. People generally choose a single language and stick with it, regardless of the consequences. For example, I watch a reasonable number of talks about system architecture, as this is something I'm involved with for [Myna](http://mynaweb.com/). The talks about scaling Ruby or Python systems almost invariably come down to replacing components with systems built in other languages, but the presenters never seem to acknowledge the glaring problem with their technology choice. The talks always end by stating that they're recruiting for more Python or Ruby developers, but they don't seem to realise that if switched to something more efficient their scalability concerns would likely be reduced.

If I had to pick one language to use it would be Scala. I believe the JVM currently offers the best combination of performance, libraries, access to OS facilities, and safety, and I believe Scala is the most expressive language on the JVM. There is a lot to learn in Scala, particularly if you're new to functional programming, but if you're in it for the long term it is a worthwhile investment.

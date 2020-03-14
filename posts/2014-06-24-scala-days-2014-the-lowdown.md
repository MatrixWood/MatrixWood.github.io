---
layout: post
title: "Scala Days 2014: The Low-down"
description: Everything you wanted to know about Scala Days 2014
category: programming
repost: underscore
---

[Scala Days 2014](http://www.scaladays.org/) wrapped up last week. There were many great talks and great conversations. Here I want to highlight some of the ... well ... highlights of the three hectic days the five of us spent in Berlin.

My favourite talk at Scala Days was [Runar's](https://twitter.com/runarorama) on the value of [free monads](https://dl.dropboxusercontent.com/u/4588997/ReasonablyPriced.pdf). The aim of the talk was to show how monads can be combined more easily than allowed by [monad transformers](http://underscoreconsulting.com/blog/posts/2013/12/20/scalaz-monad-transformers.html) by using the [free monad](http://eed3si9n.com/learning-scalaz/Free+Monad.html) and some implicit magic. The concept of free functors (free monoids, free monads, and so on) is quite simple but powerful. I hope to blog more about it in the future. For now, go read [Runar's slides](https://dl.dropboxusercontent.com/u/4588997/ReasonablyPriced.pdf).

I didn't attend [Easy Metaprogramming For Everyone!](http://scalamacros.org/paperstalks/2014-06-17-EasyMetaprogrammingForEveryone.pdf) but Dave tells me it was a fantastic talk. The summary is that a new API for macros is in development. This new API is much simpler to use -- for instance, separate compilation will no longer be necessary -- and enables better IDE support.

I did attend Dave's [talk on macros](https://github.com/underscoreio/essential-macros) and I found it an excellent introduction. I'd never written a Scala macro before Dave's talk, but following it I immediately opened my laptop and knocked out a macro I've wanted to write for a long time (for logging the current file and line). Check it out if, like me, you're new to macros and want an easy introduction.

Another talk I enjoyed was [Enterprise Brownfield: Scala to rescue](http://jmhofer.johoop.de/?p=548). The main point I took from it is that one great developer can bulldoze through any obstacles. Some believe that process can substitute for ability, but I don't hold with this. Not everybody on the team needs to be awesome, but having at least one very good developer makes a huge difference in my experience.

Of course all of this pales next to the consensus talk of the conference, Scala: The First Ten Years. Go read [the slides](http://rapture.io/tenyears/) or [watch the trailer](https://www.youtube.com/watch?v=e-DXJT8VnAA), and then weep that you, like I, missed it.

I could write a lot more about other talks I saw, but it would make for a very long blog post. Kevin Wright is [collecting slides](https://gist.github.com/kevinwright/9505828a0dcc0c4c0d56) from Scala Days so check out that link for any talks on which you want more detail.

Conferences are good on many levels, which is why we're very happy to be helping organise London's [Scala Exchange](https://skillsmatter.com/conferences/1948-scala-exchange-2014) in December. The Call for Papers has just opened, and you can expect more announcements soon.

---
layout: post
title: "Sex, Lies, and (Server-side) Javascript"
description: ""
category: programming
tags: [node, jvm, performance]
---
{% include JB/setup %}

[This](http://aphyr.com/posts/244-context-switches-and-serialization-in-node) thoughtful investigation into the performance of Node.js versus the JVM (represented by Clojure), and the [followup](http://news.ycombinator.com/item?id=4310723) comment from Node.js creator Ryan Dahl crystallized some thoughts regarding the Node.js hype-train and its intersection with reality. Within is an old lesson that anyone seeking traction with a new product would do well to remember.

Let's start with Ryan Dahl's comment on HN, which suggests a certain amount of [cognitive dissonance](http://en.wikipedia.org/wiki/Cognitive_dissonance):

> Node is popular because it allows normal people to do high concurrency servers. ... Syntax and overall vibe are important to me. I want programming computers to be like coloring with crayons and playing with duplo blocks. If my job was keeping Twitter up, of course I'd \[be\] using a robust technology like the JVM. ... Node has a large number of newbie programmers. I'm proud of that; I want to make things that lots of people use.

Let's leave aside the many arguments[^arguments] we could have about these statements, and just focus on the odd internal inconsistency. Node is popular with new programmers, and it's popular because it makes concurrency easy. Yet strangely it isn't a good fit for high concurrency applications because it isn't robust. And how many new programmers are jumping straight into concurrency? What to make of that?

[^arguments]: Oh so many arguments! Duplo composes, but CPS is practically the paradigmatic example of a global transformation. And on it goes.

In there lies, I believe, part of the secret of Node's success. When I look at my own usage of Node, I find I, surprisingly, use Node.js almost every day. Of course I don't write high concurency servers in Node -- I take Ryan's advice and use something robust. In fact I hardly ever write any Node code, but I do use [LessCSS](http://lesscss.org/), [Coffeescript](http://coffeescript.org/), and other little utilities that run on Node in my day-to-day work.

Javascript is a very popular language, and it's the first and often only language for many people. This jibes with Ryan's comment. When you're proficient with a language it's natural to want to apply it to many tasks. It's also natural, when you're a developer, to write lots of little scripts to do small chores[^chores]. Before Node there was no platform for Javascript developers to write these kind of programs. I'm sure some people do spend substantial time developing servers in Node, but my contention is that most Node programs are of the kind I use -- useful little utilities that get a relatively simple task done.

[^chores]: Sometimes these scripts even save the developer time!

If I'm correct, why all the noise about asynchronous blah blah blah? **Because sex sells!** Scripting languages have been around for a long time, but have always been marginalised. Every developer knows the real stuff, the sexy stuff, is crazy high performance code. So by positioning Node as a language for "high concurrency servers" it is seen as a "hard-core" platform, and one worth investing time in. Learning Node shows one is aspiring to reach the lofty height of high-performance guru.

This [aspirational marketing](http://en.wikipedia.org/wiki/Aspirational_brand) tactic applies to many other products as well. For example, how many car ads show driving on an empty road (or in a jungle if it's a 4x4)? How does this compare with most people's daily driving experience? The same is true to some extent with my startup, [Myna](http://www.mynaweb.com). One of Myna's selling points is it gets results faster than traditional A/B testing. This is true, and attracts a certain kind of data minded person. However, we're finding that being able to iterate faster, that is, the workflow advantages of Myna, are what people benefit the most from long term. It's not the "make more money" angle per se, but rather "try more things" that enables people to make more money long term. The former is an easier sell though, and it's well worth remembering this for whatever you're working on.

## Coda

To prevent misunderstanding let's just cover a couple of arguments I'm not making:

- I don't believe the Node developers set out to cynically market Node. I believe they really do believe Node is a great platform for high concurrency server (modulo internal inconsistency in their beliefs).
- Although I've made it fairly obvious I don't much like Node as a high-performance platform, I'm not debating its merits here. This has been done many times before in other venues.

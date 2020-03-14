---
layout: post
title: "Streaming Algorithms, Scala eXchange Edition. Or, Stop Analysing and Start Acting"
description: "Slides and video from my streaming algorithms talk at Scala eXchange 2012"
category: data
tags: [talk, ml, big-data, streaming-algorithms, online-learning]
---
{% include JB/setup %}

On Monday I delivered a talk on streaming algorithms at [Scala eXchange 2012](http://skillsmatter.com/event/scala/scala-exchange-2012). Skillsmatter are super-fast at getting video online, so you can [view it](http://skillsmatter.com/podcast/scala/real-time-analytics-in-scala) already! My [slides](/downloads/scala-exchange-2012.pdf) are also online.

Compared to previous talks I spent much more time on motivation. Analytics are only part of the build-measure-learn loop (or, if you prefer, the scientific method) and I wanted to put them in context, and, to be honest, motivate people to look beyond analytics. The focus of the big data community still seems to be on collecting data and performing elementary analyses on it. This misses the point that if your data doesn't lead to action there is no value in collecting it. Furthermore, optimising your speed through the build-measure-learn loop can be a huge win. The best way to speed up a process is to automate it and, as we're showing with [Myna](http://mynaweb.com/), this is entirely viable. I truly believe that if data scientists are to realise their true value they need reposition themselves as a stage in the feedback loop, to a critical component overseeing and optimising the entire loop.

---
layout: post
title: "CRDTs for fun and eventual profit"
description: ""
category: programming
lead:
repost: underscore
tags: talk
---

At [Velocity](http://velocityconf.com/velocityeu2013/public/schedule/detail/31058) I gave a talk on conflict-free replicated datatypes, or CRDTs for short. It wasn't the best received talk I have ever given; it was my first time at Velocity and I misjudged the audience. However I have had a chance to redeem myself at [Scala eXchange](http://skillsmatter.com/event/scala/scala-exchange-2013), where I gave what I think is a much better (and funnier) talk. Slides are [here](/downloads/scala-exchange-2013-crdt.pdf) or you can watch [the video](http://skillsmatter.com/podcast/home/how-do-we-reconcile-eventually-consistent-data), If you want the Velocity edition, the slides are [here](velocity-2013-crdt.pdf). The talks are mostly the same in terms of content, but there are differences in organisation and layout.

At this point you might be wondering why you should invest your time learning about CRDTs, so let me give here the motivation.

CRDTs are a way of handling replicated or distributed data. What is distributed data? It just means data that is copied to many machines. As soon as we have such a distributed system we have to think about what happens when our data changes. We can decide that all machines will be aware of all changes.  That is, we can maintain *consistency*. This is nice because it means we never deal with out-of-date data, but it requires every change to be sent to every machine before it is considered complete. If a machine (or the network) goes down we must refuse updates because we can't ensure everyone has seen every update, and thus we can't maintain consistency .

We can instead prefer *availability*, meaning we'll just soldier on if machines go down, but this does mean we will end up with the same piece of data having different values on different machines. In other words our data will become inconsistent. CRDTs allow us to recover a particular type of consistency, called eventual consistency, without a great deal of work. With CRDTs we will be able to merge different copies of our data together without issue, and if we merge all copies together we are guaranteed to arrive at the same value everywhere.

So, why not just enforce consistency? I've given one argument above: to have a site that doesn't go down. There are more good reasons. The one I presented in my talks is to reduce latency.

Studies by [Google](http://static.googleusercontent.com/media/research.google.com/en//pubs/archive/34439.pdf) and [many others](http://highscalability.com/latency-everywhere-and-it-costs-you-sales-how-crush-it) have shown that increased web site latency correlates directly with user abandonment. In other words, slow web sites make less money.

Well implemented code can generate a response very quickly. For example, [Myna's](http://mynaweb.com/) response time averages about 1ms with 99th-percentile about 7.5ms. However the network latency dwarfs this value. The speed of light sets a hard limit on how quickly data can travel -- it takes light about 134ms to travel around the globe. Real networks are much slower than this, particularly when you get to ADSL or mobile networks. We have found the ping time from London to New York is usually less than 100ms, but the time to a random US user is often 500ms or more.

The only way to make a dent in network latency is to move data closer to the client. The infrastructure to do this is in-place. AWS has eight data centres worldwide, for example, which puts them close enough to the majority of the Internet using population. We can go a step further and run things directly in the user's hand -- -n their mobile device -- and effectively zero latency.

If we go this route we very definitely have a consistency problem. We can't wait for a distributed transaction to commit whenever we change data, because that adds back the latency we're trying to remove (particluarly on an offline mobile device!) Instead we must accept inconsistent data, and have some way to resolve these inconsistencies when they arise.

Vector clocks are the classic method for handling this problem, and are found in eventually consistent databases like Riak and Amazon's Dynamo. Vector clocks store all the different versions of the data, along with a version number -- the vector clock -- that describes the relationship between the versions. It's a fair complaint that vector clocks don't solve the problem at all. Rather, they punt it to the programmer but at least give enough information to resolve the issue.

The CRDT approach is much nicer. CRDTs provide a merge operation -- given two copies of a CRDT we can merge them together to create a new CRDT. Merging is guaranteed to converge to the correct answer. More precisely, if we stop all updates to the system and merge together all copies, we're guaranteed that we will arrive at the correct answer everywhere no matter what order we apply our merges in. This makes inconsistency very simple to deal with. Whenever we find inconsistent data we can simply merge and we know we're getting closer to the correct answer.

In summary, CRDTs make replicated data simpler to manage, which has real benefit for anyone who is concerned about data synchronisation, reliability, or latency. If that piques your interest watch [the video](http://skillsmatter.com/podcast/home/how-do-we-reconcile-eventually-consistent-data) or read [the slides](/downloads/scala-exchange-2013-crdt.pdf) for the details of how CRDTs work.

---
layout: post
title: "Pre-register for the Streaming Algorithms Course"
description: ""
category: data
tags: [online-learning, streaming-algorithms, teaching, ml, big-data]
lead: I wrote this post to gauge interest in a course on streaming algorithms. I didn't get the response I wanted, so the course is on the back-burner.
---
{% include JB/setup %}

<div class="well">
I didn't get the response I wanted to run the course. I still hope it will go ahead some day. If you're interested, sign up to my newsletter and you'll receive announcements (and other interesting content) there.
</div>

I have given [a](/streaming-algorithms/2012/11/22/streaming-algorithms-scala-exchange-edition) [number](/streaming-algorithms/2012/10/01/strata-slides) [of](/streaming-algorithms/2012/09/14/lsug-slides) [talks](/streaming-algorithms/2012/08/29/lean-data) on streaming algorithms and had requests for more depth on the material. I would like to expand my talks into a course, but in a true data-driven way I first want to gauge interest. Hence I'm asking interested people to [pre-register](https://docs.google.com/forms/d/1ajnHie3QHy13AvOU8ivYVcjG9ERHdq_09QrIFc8BziQ/viewform) now (no monetary commitment required) so I know if it's worthwhile going ahead.

Go ahead and [pre-register](https://docs.google.com/forms/d/1ajnHie3QHy13AvOU8ivYVcjG9ERHdq_09QrIFc8BziQ/viewform) now or read on for more on the course.

## Why?

I believe streaming algorithms are a wildly underappreciated technology for data analysis.

The defining characteristic of a streaming algorithm is that it only processes a data point once. You can run them on data stored on disk, but more commonly you just fire data at the algorithm as it arrives.

Streaming algorithms are real-time by definition. Real-time is a very nice property to have. Obviously, it lets you know right away what's going on in your system, which can be important in certain situations. An underappreciated benefit is you don't have any task switching. Just like long compile times lead to distracted developers, if queries take a long time to run you'll end up reading your email for half an hour before you realise it.

Streaming algorithms also tend to be ridiculously scalable. For example, we'll look at algorithms that use only 4K to count the number of distinct items in a set with 10^9 elements. Scalability is great even if you don't have a tidal wave of data, because when things fit into a single machine they are so much easier to develop and maintain.

Finally, streaming algorithms are also easy to implement (and you can often find implementations online). They are the kind of thing you can knock up in an afternoon. Then, because of their awesome scalability, just wrap a HTTP front-end around it and you have yourself an analytics machine. You'll spend two days instead two months building a system, which is really awesome.

## Course Content

The course will run in two parts. The first part will cover methods for processing streams of numbers. These are mainly used in system monitoring scenarios. We'll start by looking at various ways of calculating moving averages, useful for calculating hits per second over a time window and so on. Then we'll move onto quantiles, which you'll typically want to use for calculating 99% response time etc. Finally we will look a methods for constructing histograms from streaming data, useful if you want to get a closer look at your response time distribution, for example.

The second part of the course will focus on methods for sets and multi-sets. These are better suited to answering business questions. We'll start with ways to find the most frequent items in a stream. You can use this to find who your most active users are, or to, say, filter out attackers by IP address. We will then look at methods to calculate the number of distinct items in a set. This is super useful for general analytics. Say you're running a website. You can answer a lot of questions if you can count the number of people who visit different parts of your site and count the number of people who arrive from different sources. This is exactly what these methods do. Even better, they also support set algebra, so you can find how many users are in the intersection, union, and set difference of the various sets you're counting. As these methods are super scalable you count thousands of different sets on a single box, which can get you a very powerful and flexible system.

The course will be very practical. Although we'll be focused mostly on algorithms, and thus be programming language agnostic, we'll talk implementation details and I'll give example code (probably in Scala, Python, or Javascript). You can use whatever language you want. So long as you can perform bit manipulation you'll be alright.

## Course Options

I'd like to offer two options for the course: in person, and online. The former will run in London over a day and cost less than £500 per person. The online course will be self paced and cost less than £100.

Once again, if you're interested tell me by [signing up](https://docs.google.com/forms/d/1ajnHie3QHy13AvOU8ivYVcjG9ERHdq_09QrIFc8BziQ/viewform) and I'll let you know if and when the course goes ahead.

<div class="well">
I didn't get the response I wanted to run the course. I still hope it will go ahead some day. If you're interested, sign up to my newsletter and you'll receive announcements (and other interesting content) there.
</div>

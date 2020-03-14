---
layout: post
title: "Probabilistic Programming in Scala"
author: "Noel Welsh"
---

At the [Typelevel Summit in Philadelphia][typelevel-philly] I gave a talk about probabilistic programming, which I have recently been [exploring][pfennig]. Probabilistic programming combines two great research areas that go great together---functional programming and machine learning (specifically, Bayesian inference). In this blog post I'll attempt to explain the basic ideas behind probabilistic programming. I'm assuming you, dear reader, skew more towards programming than statistics, but are not afraid of numbers. Hence I'll concentrate more on the programming than the machine learning side of things here.

<!--more-->

## Probabilistic Models and Inference

To understand probabilistic programming we need to first understand probabilistic models and inference. A probabilistic model describes how things we can observe are dependent on things we cannot observe. We usually assume this is not a deterministic relationship and hence the dependency between the observables and the unobservables is governed by some probability distribution. This type of model is sometimes called a generative model.

Imagine a doctor predicting an illness. The doctor has many symptoms, such as temperature, skin rash, shallow breathing, and so on, from which they can make an educated guess about the underlying condition that is causing the symptoms. Of course this is not a perfect process. Several different illnesses may cause very similar symptoms, and a given illness doesn't always cause the same symptoms, so it makes sense to model these relationships in terms of probability distributions.

Let's look at another example in a little more detail. Imagine we want to categorise documents by the topic they discuss, perhaps so we can recommend relevant reading to people. We might model a document as the following generative process:

1. choose some topics and per-topic weights;
2. each topic defines a distribution over words;
3. from these distributions over words we choose the actual words we read on the page, in proportion to the per-topic weights.

This particular type of model is known as a [topic model][topic-model] and has many applications. Some of the more unusual include [categorising aesthetic preferences][etsy-lda] at Etsy, and attempting to determine the [use of rooms in the preserved households in Pompeii][topic-model-pompeii].

Hopefully these brief sketches are enough to convey the idea of probabilistic models. You can imagine many other generative processes, such as the properties of an anstronomical object influencing the emissions we observe, a user's interests determining their online behaviour, and a NGO's activities effecting health outcomes in their region of operation.

Generative models work well when we have a prior knowledge of how the world works that we can bring to bear in constructing the model. This is the case in most fields of scientific study, since the goal of science is to create such knowledge. These are also fun to work with because we can use them to generate dummy data, such as [machine-generated conference papers][scigen] to confuse and amuse our peers.

Once we have created a probabilistic model (and finished amusing ourselves creating dummy data) we likely want to use our model to perform *inference*. Inference is the process of running the model "backwards". That is, using some real data we have observed to guess what might be the state of things we cannot observe. For example, we could use the text of a document to make an informed guess at its topics, or use our astronomical observations to choose the best candidate solar systems for intelligent life outside our own. Since we can't be certain of the state of the unobserved elements in our model we might want to have a probability distribution over the possibilities. Constructing such a distribution is what distinguishes *Bayesian inference* from alternatives such as maximum likelihood inference that only find a single state of the unobserved properties.


## Motivating Probabilistic Programming

We've now very briefly sketched generative models and described the problem of Bayesian inference. This is a well established field within statistics and machine learning and over time researchers have noticed a few things:

- it's quite easy to construct a generative model; and
- these models have the same kind of structure; but
- writing an inference algorithm for each particular model is slow and error-prone.

Wouldn't it be great if we could derive an inference algorithm given a model description? We can think of this an analogous to writing in assembly versus a high-level language. When we write assembly we optimise everything by hand, implementing our own control structures and so on tailored to our specific problem. This is the situation with creating a custom inference algorithm for a particular generative model. When we write in a high-level language we use predefined constructs that the compiler translates into assembly for us. We gain a lot of productivity by giving up a little bit of performance (which we often don't miss). This is the goal with probabilistic programming.


## It is a Monad, of Course

The goal of probabilistic programming sounds true and noble, but if we are going to achieve it we must first work out how to structure our generative models. In what will be no surprise to seasoned functional programmers, we will use a monad. Let me explain how this comes about.

Recall the example we had of a generative model for creating a document. A document is generated by:

1. choosing some topics and per-topic weights;
2. each topic defines a distribution over words;
3. from these distributions over words we choose the actual words we read on the page, in proportion to the per-topic weights.

We can annotate this with types to illustrate how this problem might work. We use `Distribution[A]` to represent a distribution over some type `A`. Now a document is generated by:

- choosing some topics---this could give us a `Distribution[Seq[Topic]]` (or just `Distribution[Topic]` for a simpler model).
- from each topic we get a distribution over words---this is a function `Topic => Distribution[Words]`.
- from these distributions over words we choose the actual words we read on the page---this is the process of sampling from a `Distribution[Words]`.

(We're trying to balancing simplicity vs accuracy here, so our model is not considering grammar or how many words make up a document. Hopefully you can fill in the gaps, but if you're having trouble check out my talk which uses a simpler and complete example.)

The core part of this model is where we connect `Distribution[Topic]` and `Topic => Distribution[Words]` to create the `Distribution[Words]` from which we can construct a document. Symbolically, what do we replace `???` with to make the following equation hold?

``` scala
Distribution[Topic] ??? Topic => Distribution[Words] = Distribution[Words]
```

The answer is `flatMap` and thus we have a monad. (You can check the monad laws hold as well, if you don't trust me, though we need to define the semantics of `flatMap` in this case. See below.)

If you've ever used ScalaCheck or similar systems you'e used the probability monad.


## Deriving Inference Algorithms

There are many ways we can implement a monad for probability. If we deal only with discrete domains we can represent everything internally as a `List[(A, Probability)]` (where `Probability` might be a type alias for `Double`) and compute results exactly. This isn't so useful for real applications, as we'd like to deal with continuous domains (or even large discrete domains, as we exponentially increase the size of our representation on each `flatMap`). You can read an implementation [here][enumeration].

We could use a representation based on a sampling. This works with continuous domains, and the user can create as many samples as needed to obtain the accuracy they desire. However this is still leaves something to desire. We have many years of research on inference algorithms, which are analogous to compiler optimisations, and we'd really like the system that runs our probabilistic programs to analyse the structure of the program and apply the best inference algorithm for what it finds. Just like we do in a compiler, this motivates representing our probabilistic program as an abstract syntax tree that can then be manipulated by optimisation algorithms. As regular readers of the blog will know, the [abstract syntax from a monad][free-monad-ast] is just the free monad. In Cats we can literally represent probabilistic programs with the type `Free[Distribution, A]` where `Distribution` is something from which we can sample.

Now we can actually derive our inference algorithms, and this is where I'm going to get hand-wavey for two reasons: I'm assuming you're not so familiar with machine learning and thus discussing inference algorithms probably wouldn't be very meaningful to you, and more importantly, this is still an open area of research. Current systems implementation general purpose inference algorithms but little in the way of problem-specific optimisations. The general principle is, just like a compiler, the more information you retain the more optimisation you can perform, but at the same time most optimisations are only useful for a small number of programs. It is very much an open issue how much we can rely on general purpose inference algorithms and computing horse-power to perform inference in sufficient time versus how much we need to use clever maths to optimise specific cases.


## Conclusions and Future Work

I'm very excited by probabilistic programming because I think it will greatly reduce the cost of inference, and I believe there are many fields that will benefit from this---I gave a few examples above. For most fields results are not time critical, and thus I'm more interested in exploring general purpose inference algorithms and parallel / distributed implementations right now than I am in using clever maths. However I really need to see how things work in practice first, and for that I need people with actual problems to solve.

Probabilistic programming also fits very squarely into my interests in machine learning and programming languages. If you're interested in learning more, check out [my talk][pp-talk], [my slides][pp-slides], or [my code][pfennig]. If you have an interest or application for probabilistic programming do get in touch---I would love to know about practical problems this could help solve!


[typelevel-philly]: http://typelevel.org/event/2016-03-summit-philadelphia/
[pfennig]: https://github.com/noelwelsh/pfennig
[pp-talk]: https://www.youtube.com/watch?v=e1Ykk_CqKTY&index=3&list=PL_5uJkfWNxdkQd7FbN1whrTOsJPMgHgLg
[pp-slides]: http://noelwelsh.com/downloads/typelevel-summit-philly-2016.pdf

[topic-model]: https://www.cs.princeton.edu/~blei/topicmodeling.html
[etsy-lda]: http://mimno.infosci.cornell.edu/info6150/readings/p1640-hu.pdf
[topic-model-pompeii]: http://mimno.infosci.cornell.edu/papers/pompeii.pdf
[scigen]: https://pdos.csail.mit.edu/archive/scigen/
[enumeration]: https://github.com/noelwelsh/pfennig/blob/master/src/main/scala/pfennig/Enumeration.scala
[free-monad-ast]: {% post_url 2015-04-14-free-monads-are-simple %}

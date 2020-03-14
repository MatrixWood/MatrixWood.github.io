---
layout: post
title: "COLT and ICML"
description: "A quick overview of my highlights from COLT and ICML 2012"
category: data
tags: [ml, icml, colt, bandit, topic-model, online-learning]
---
{% include JB/setup %}

I spent last week in the lovely city of Edinburgh attending [COLT](http://www.ttic.edu/colt2012/) and [ICML](http://icml.cc/2012/). I'm still digesting a lot of what I saw, but I thought it might be interesting to give a quick list of my highlights.

## Modern Banditry

Bandit and reinforcement learning algorithms are of major interest, given my work on [Myna](http://mynaweb.com/), and both conferences featured interesting papers on these problems. Of particular note, [Hierarchical Exploration for Accelerating Contextual Bandits](http://icml.cc/2012/papers/933.pdf) is an algorithm I could see us putting into practice very soon. This solves the problem of training a (linear) classifier under bandit feedback by initially restricting the parameters to a lower-dimensional subspace, and then relaxing this restriction as more data arrives. This reduces the amount of time spent exploring, and hence the regret, so long as a good subspace can be identified.

[The Best of Both Worlds: Stochastic and Adversarial Bandits](http://jmlr.csail.mit.edu/proceedings/papers/v23/bubeck12b/bubeck12b.pdf) is a very interesting paper, showing it is possible to switch between a stochastic and adversarial bandit and still bound the regret if either case is true. I think this work is very early, and hopefully we'll see further developments in the future. In particular bandit algorithms for the non-stationary but not adversarial case are of interest but largely unexplored in the literature.

The [Policy Regret](http://icml.cc/2012/papers/749.pdf) and [Local Regret](http://icml.cc/2012/papers/803.pdf) papers also presented some interesting ideas rethinking how we define regret. Finally there are number of reinforcement learning papers, which I haven't yet had time to read.

## Topic Models

[Topic models](http://en.wikipedia.org/wiki/Topic_model) continue to be a hot topic as they have direct application in many content recommendation systems. [Latent Dirichlet allocation](http://en.wikipedia.org/wiki/Latent_Dirichlet_allocation) is one basic topic model, and there has been much work scaling it to large document collections. Previous work includes [online LDA](http://www.cs.princeton.edu/~blei/papers/HoffmanBleiBach2010b.pdf)
and [Yahoo's Hadoop-based LDA](https://github.com/shravanmn/Yahoo_LDA). At ICML we were treated to [Sparse stochastic inference for latent Dirichlet allocation](http://icml.cc/2012/papers/784.pdf). This uses a hybrid variational/sampling scheme to do online inference (that is, it processes a stream of data). The experiments are performed on a 33 billion word corpus, which demonstrates the algorithms scalability. Of course this is not the last word on scaling LDA. In the future we can expect [spectral methods](http://arxiv.org/abs/1204.6703) to provide an alternative learning paradigm. In this particular case the algorithm is based on the singular value decomposition, which is [surprisingly easy to scale](http://www.stanford.edu/group/mmds/slides2010/Martinsson.pdf).

Speeding up LDA is not the only game in the topic model town. The other main direction is developing more expressive or domain specific models. ICML had plenty of these, including models for [melodic sequences](http://icml.cc/2012/papers/585.pdf), [labelled data](http://icml.cc/2012/papers/387.pdf), [changing topics](http://icml.cc/2012/papers/476.pdf) and [more](http://icml.cc/2012/papers/113.pdf).

While gathering the links for this post I came across [this paper](http://www.cs.cmu.edu/~amahmed/papers/UserModeling_KDD11.pdf), which looks very very interesting if topic modelling is your bag.

## Other Things

There was plenty of work on optimisation and Bayesian inference. I use these techniques, but they're not something I have a particular interest in so I don't have much to say here.

Both ICML and COLT had many papers on recommendation systems (also known as collaborative filtering, or matrix prediction/completion). I haven't got around to reading most of these, but I'll definitely be going through them at some point.

Finally, three papers that don't fit any of the above themes but I found interesting:

1. [Exact Soft Confidence-Weighted Learning](http://icml.cc/2012/papers/86.pdf) is the latest in the series of online linear classifiers that began with the passive-aggressive perceptron. I really like these algorithms; they are simple to implement, give great performance, and being online you don't need to keep your training data around. I'm looking forward to testing this one.

2. Sequence prediction is a problem that has many applications, from compression to recommender systems. [Learning the Experts for Online Sequence Prediction](http://icml.cc/2012/papers/471.pdf) shows how to learn a suffix-tree variant and demonstrates good performance predicting browsing behaviour.

3. Most developers know about [k-d trees](http://en.wikipedia.org/wiki/K-d_tree) for storing spatial data. K-d trees aren't efficient in high dimensions as they branch too often. [Approximate Principal Direction Trees](http://icml.cc/2012/papers/348.pdf) describes an algorithm that represents high dimensional data compactly and doesn't take much time to compute. The key insight, for me, is to use only a few iterations of the [power method](http://en.wikipedia.org/wiki/Power_iteration) to compute a vector that is with high probability in a direction of high variance.

If you read this you might also be interested in [Alexandre Passos' reading list](http://atpassos.posterous.com/icml-2012-reading-list).

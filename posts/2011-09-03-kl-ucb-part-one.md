---
layout: post
title: "KL UCB Part One"
date: 2011-09-03 21:38
comments: true
category: data
---

If you've read this blog so far, you're probably thinking "where's the maths?" So, let's get started.
Today's paper is **[The KL-UCB Algorithm for Bounded Stochastic Bandits and Beyond](http://arxiv.org/pdf/1102.2490v4)**.

## Introduction

We all know and love the multi-armed bandit problem, not least because [Myna](http://www.mynaweb.com) is based on it. The stochastic variant (typically) boils down to estimating a confidence region for the expected reward of each arm. Better algorithms have better ways of estimating this confidence region. The KL-UCB algorithm is a new algorithm that seems to have a particularly good way of estimating the confidence regions.

### Notation

 - Time `\( t = 1, 2, \ldots, n \)`
 - Choice of arm at each `\(t\)`, `\(A_t \in \{1, \ldots, K \}\)`
 - Rewards `\( X_t \)` are i.i.d. conditional on `\( A_t \)` with expectation `\( \mu_{A_t} \)`
 - Policy is the (possibly randomised) decision rule that given the history of interactions `\( (A_1, X_1, A_2, X_2, \ldots, A_{t-1}, X_{t-1}) \)` choses the next action `\( A_t \)`
 - The best arm `\( a^* \)` has expected reward `\( \mu_{a^*} \)`
 - The aim is to minimise **regret** `\( R_n \)`, the difference between the sum of reward up to horizon `\( t = n \)` and the reward that could have been accumulated if only the optimal arm had been chosen.

### Fundamentals

The definition of reward (i.i.d.) means we're dealing with a **stochastic bandit problem** (cf non-stochasitc and non-stationary problems). Within this class of bandit problemss there are **parametric** and **non-parametric** variants. In the parametric case we assume `\(X_t\)` given `\(A_t = a\)` belongs to some parametric family `\( \{ p_\theta, \theta \in \Theta_a \} \)`. In this case a lower bound on performance, and an optimal policy, have been known [since the dawn of time](http://www.sciencedirect.com/science/article/pii/0196885885900028). Somewhat later, a lower bound was proven on the number of times a sub-optimal arm is chosen. Let `\(a\)` be a sub-optimal arm with parameters `\(p_{\theta_a}\)`, `\(n\)` the time step, and `\(N_a(n)\)` the number of times arm `\(a\)` is chosen. Then,

`\[ N_a(n) \geq \left( \inf_{\theta \in \Theta_a: E[p_\theta] > \mu_{a^*}}  \frac{1}{KL(p_{\theta_a}, p_\theta)} + o(1) \right) log(n) \]`

It's worth spending some time unpacking this statement. The _infimum_ is, informally, the minimum value of the set. So the expression `\(\inf_{\theta \in \Theta_a: E[p_\theta] > \mu_{a^*}}\)` is finding setting of the parameters that give an arm with expected reward as close as possible to (while still being greater than) the optimal arm. `\( KL(p_1, p_2) \)` is the _Kullback-Leibler_ divergence between distributions `\(p_1\)` and `\(p_2\)`. It's not a true metric but we can think of it measuring the distance between two distributions in some sense. So we're measuring the "distance" between this sub-optimal arm and our approximation to the optimal arm. So basically what this expression ends up saying is we'll play an arm with frequency proportional to its similarity to the optimal arm. Makes sense: obviously bad arms don't get played as much, but arms that look like the optimal arm do.

For this we can easily derive a lower bound on regret:

`\[ \lim_{n \rightarrow \infty}\inf \frac{\mathbb{E}[R_n]}{log(n)} \geq \sum_{a : \mu_a < \mu_{a^*}} \inf_{\theta \in \Theta_a: E[p_\theta] > \mu_{a^*}} \frac{\mu_{a^*} - \mu_a}{KL(p_{\theta_a}, p_\theta)}  \]`

In the non-parametric case we only assume the rewards are bounded. KL-UCB is a non-parametric algorithm, yet still manages match the lower bound for the parametric case when the rewards are binary _and_ can be extended to a large class of parametric policies.

KL-UCB is an **upper confidence bound** algorithm. As the names suggest these algorithms compute an upper confidence bound on the reward of each arm, and choose the arm that has the highest bound at each time step. This approach is sometimes called _optimism in the face of uncertainty_, which is a catchy phrase but we should remember that this phrase is derived from the development of optimal algorithms not vice versa.

## The KL-UCB Algorithm

We'll start by introducing the algorithm and then delve into how and why it works. The algorithm is simple to state. We define:

 - The number of times an arm `\(a\)` has been chosen is `\(N[a]\)`
 - The total reward an arm `\(a\)` has received is `\(S[a]\)`
 - The Bernoulli KL divergence `\(d(p,q) = p \log \frac{p}{q} + (1 - p) \log \frac{1-p}{1-q} \)`
 - A real non-negative parameter `\(c\)`, which is recommended to set to 0

Then:

 1. Play each arm once to initialise it
 2. Once all arms are initialised, we calculate an upper confidence bound given by:

    `\[ \max \left\{ q \in [0,1] : N[a] d \left( \frac{S[a]}{N[a]}, q \right) \leq \log(t) + c\log(\log(t)) \right\} \]`

  and play the arm with the highest confidence bound

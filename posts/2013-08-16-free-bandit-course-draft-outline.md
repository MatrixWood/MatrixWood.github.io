---
layout: post
title: The Free Bandit Course &mdash; Draft Outline
category: data
list_cta: Sign up for my free course on bandit algorithms.
---

In a [previous post]({% post_url 2013-08-05-why-you-should-know-about-bandit-algorithms %}) I argued for the usefulness of bandit algorithms to marketers and data scientists. In short, they allow us to perform faster A/B testing, to personalise offers to customers, and to recommend rapidly changing content. I also mentioned I'm running a free course on bandit algorithms. It is starting next week, and I've been busy working on the course material. I now have a clearer idea of the course outline and how the material will be delivered, which I'd like to share with you.

(If you're interested in the course and just want to sign up, scroll to the bottom of the page.)

## Goal

The overarching goal is to describe the major themes and results in the world of bandit algorithms, as they apply to practitioners. This means emphasising concepts, and deemphasising the mathematics. I'll provide lots of links if you want to get your proof on. Most of my examples will be drawn from the Internet world, because that is what I'm most familiar with and what I imagine is more relevant to you.


## Course Outline

We'll start by looking at what problem a bandit algorithm is trying to solve. We'll see there is a family of related problems that are all called bandit problems. We'll discuss where we might encounter these problems in practice, focusing on Internet applications.

Once we've established what a bandit algorithm is, we'll look at a really simple example of one. Analysing the behaviour of this algorithm leads us to the core problem of bandit algorithms, which is balancing exploration and exploitation. We'll discuss our first workable algorithm, e-greedy, which provides a very simple way to balance these two concerns.

My current plan is discuss hypothesis testing -- the technology behind classic A/B tests -- in a reasonable amount of depth. We'll cover type I and II errors, p-values, power, and sample size, and the exact meaning of statistical significance. This might be too much -- what do you think? We'll then look at the bandit analogue of A/B testing, known as "best arm identification,"" and see how modern algorithms can improve on hypothesis testing.

From here we can explore some of the practical issues with running bandit algorithms: choosing a goal, dealing with delayed rewards, and investigating the effect of changing conversion rates on the algorithms.

Having looked at the best arm identification problem we then turn to the "regret minimization" problem, which is the best known bandit problem. We'll look at algorithms for this and examine how they compare to the best arm identification algorithms we looked at above.

I'm not 100% sure on the A/B testing to regret minimization sequence, so I might rearrange this a bit.

This covers all the basic material, but there are two other areas I'd like to look at.

The first is handling conversion rates that are changing very quickly. This is the case in, for instance, news stories. The news of the hour has a short shelf life (about an hour, I hear.) How can we adapt bandit algorithms to deal with this? We will look relatively quickly at a few approaches.

Then we'll turn to the topic of personalisation. Here we assume we have a profile for our visitors, and we want to use this in our algorithm to improve suggestions. We'll discuss some of the ways to do this, without getting too deep into the issues of machine learning.

That's everything I intend to cover. There are lots of topics I've left off: combinatorial bandits, linear bandits, and more. I feel the above are the core topics, and also what I know the most about (which helps).

## Sign Up

If you're interested in the course, sign up below. It's free and starting soon. Hope to see you there!

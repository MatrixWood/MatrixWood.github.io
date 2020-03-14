---
layout: post
title: Choosing Goals for A/B Testing
category: data
repost: myna
---

One of the most important decisions when designing an A/B test is choosing the goal of the test. After all, if you don't have the right goal the results of the test won't be of any use. It is particularly important when using [Myna](http://mynaweb.com) as Myna dynamically changes the proportion in which variants as displayed to maximise the goal.

So how should we choose the goal? Let's look at the theory, which tells us how to choose the goal in a perfect world, and then see how that theory can inform practice in a decidedly imperfect world.


## Customer Lifetime Value

For most businesses the goal is to increase customer lifetime value (CLV). What is CLV? It's simply the sum of all the money we'll receive in the future from the customer. (This is sometimes known as predictive customer lifetime value as we're interested in the revenue we'll receive in future, not any revenue we might have received in the past.)

If you can accurately predict CLV it is a natural goal to use for A/B tests. The performance of each variant under test can be measured by how much they increased CLV on average. Here's a simple example. Say you're testing calls-to-action on your landing page. The lifetime values of interest here are the CLV of a user arriving at your landing page who hasn't signed up, and the CLV of a user who has just signed up. If you have good statistics on your funnel you can work these numbers out. Say an engaged user has a CLV of $50, 50% of sign-ups go on to become engaged, and 10% of visitors sign up. Then the lifetime values are:

- for sign-ups `$50 \(\times\) 0.5 = $25`; and
- for visitors `$25 \(\times\) 0.1 = $2.50`.

The great thing with CLV is you don't have to worry about any other measures such as click-through, time on site, or what have you -- that's all accounted for in lifetime value.


## Theory Meets Practice

Accurately predicting CLV is the number one problem with using it in practice. A lot of people just don't have the infrastructure to do these calculations. For those that do there are other issues that make predicting CLV difficult. You might have a complicated business that necessitates customer segmentation to produce meaningful lifetime values. You might have very long-term customers making prediction hard. I don't need to go on; I'm sure you can think of your own reasons.

This doesn't mean that CLV is useless, as it gives us a framework for evaluating other goals such as click-through and sign-up. For most people using a simple to measure goal such as click-through is a reasonable decision. These goals are usually highly correlated with CLV, and it is better to do testing with a slightly imperfect goal than to not do it at all due to concern about accurately measuring lifetime value. I do recommend from time-to-time checking that these simpler goals are correlated with CLV, but it shouldn't be needed for every test.

CLV is very useful when the user can choose between many actions. Returning to our landing page example, imagine the visitor could also sign up for a newsletter as well as signing up to use our product. Presumably visitors who just sign up for the newsletter have a lower CLV than those who sign up for the product, but a higher CLV than those who fail to take any action. Even if we can't predict CLV precisely, using the framework at least forces us to directly face the problem of quantifying the value of different action.

This approach pays off particularly well for companies with very low conversion rates, or a long sales cycle. Here A/B testing can be a challenge, but we can use the model of CLV to create useful intermediate goals that can guide us. If it takes six months to covert a visitor into a paying customer, look for other intermediate goals and then try to estimate the CLV of them. This could be downloading a white paper, signing up for a newsletter, or even something like a repeat visit. Again it isn't essential to accurately predict CLV, just to assign some value that is in the right ballpark.


## Applying CLV to Myna

So far everything I've said applies to general A/B testing. Now I want to talk about some details specific to [Myna](http://mynaweb.com/). When using Myna you need to specify a reward. For simple cases like a click-through or sign-up, the reward is simply 1 if the goal is achieved and 0 otherwise. For more complicated cases Myna allows very flexible rewards that can handle most situations. Let's quickly review how Myna's rewards work, and then how to use them in more complicated scenarios.

Rewards occur after a variant has been viewed. The idea is to indicate to Myna the quality of any action coming from viewing a variant. There are some simple rules for rewards:

- any reward must be a number between 0 and 1. The higher the reward, the better it is;
- a default reward of 0 is assumed if you don't specify anything else; and
- you can send multiple rewards for a single view of a variant, but the total of all rewards for a view must be no more than 1.

Now we know about CLV the correct way to set rewards is obvious: rewards should be proportional to CLV. How do we convert CLV to a number between 0 and 1? We recommend using [the logistic function](http://en.wikipedia.org/wiki/Logistic_function) to guarantee the output is always in the correct range. However, if you don't know your CLV just choose some numbers that have the correct ranking and roughly correct magnitude. So for the newsletter / sign-up example we might go with 0.3 and 0.7 respectively. This way if someone performs both actions they get a reward of 1.0.

That's really all there is to CLV. It a simple concept but has wide ramifications in testing.

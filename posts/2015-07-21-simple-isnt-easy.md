---
layout: post
title: Simple Isn't Easy
author: Noel Welsh
category: programming
---

I recently wrote about [keeping Scala simple][scala-simple]. That's something you have to work at. Let me explain.

While packing up to move house I came across my undergrad vector calculus notes. Although I, sadly, don't remember the definitions of [div, grad, and curl][operators] I do remember that course as marking a turning point in my academic career.

<!--more-->

Like many academically inclined people I had cruised through high-school, always able to pick up whatever I needed to know without working very hard. When I got to University the jig was up. The material was difficult enough that I couldn't just absorb it by sitting in lectures---I had to actually work! You can imagine my shock.

It took nearly failing first year maths for the message to get through. In vector calculus the next year, I made a habit of heading straight to the library after lectures to write notes and work through exercises. I found the material difficult, as did all my friends, but then an amazing thing happened. At some point I got it, and what had seemed a dense jungle of definitions and notation became a simple and elegant arrangement of concepts.

I've had that experience many other times since. Learning functional programming from [SICP][sicp], studying Bayesian statistics, and tackling Markov chain Monte Carlo methods are some of the occasions that stick out in my memory. Every time the process has been the same: confusion, concentrated study, and then understanding and clarity.

Functional programming (via Scheme in my case) is an example particularly relevant to this blog. One feature of FP languages is that they are *expression-oriented*, meaning that most program components evaluate to a value. In imperative languages, `if` is typically a statement that does not yield a value, and functions only yield a value if they include a `return` expression. Expression-oriented languages do away with this. This is undeniably a simpler model, with fewer components and rules to remember, but it isn't easy to adopt if you have spent years with the imperative model. Sadly, in programming circles a lot of language discussions revolve around appeals to "intuitiveness", which is usually a short-hand for "works like other things I know", even if those other things have complex models. As an industry this greatly holds us back.

When I argue for [keeping Scala simple][scala-simple], I'm talking about the kind of simple that builds from a few core concepts. That doesn't mean writing simple code is easy! It's much more about an approach to design and problem solving than it is about language features, and it is quite a different approach to what you may have learned if you come from an object-oriented background. Ultimately it is worth, though. Tunneling through the learning barrier will get you to the place where it makes sense and the simplicity is evident.

[operators]: https://en.wikipedia.org/wiki/Vector_calculus_identities#Operator_notations
[sicp]: https://mitpress.mit.edu/sicp/
[scala-simple]: {% post_url 2015-06-25-keeping-scala-simple %}

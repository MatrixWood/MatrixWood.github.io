---
layout: post
title: "Slides and Code from the Interpreters Workshop at Scala Exchange 2015"
author: "Noel Welsh"
---

At Scala Exchange 2015 I gave a workshop on building interpreters in Scala. Code and slides are [here][workshop-code], though future development will take place [here][ei-code]. The workshop covered untyped interpreters, GADTs, and ended with the free monad and free applicative.

<!--more-->

So, why interpreters? The big idea here is to separate the representation of the computation from the code that runs it. This has many uses in functional programming (it underlies [the approach to functional IO][monadic-io] for example) but the most compelling reason to me is that it solves difficult problems. There are some examples in the slides:

- [feature gating at Instagram][feature-gating];
- [Spark's query compiler][spark-catalyst]; and
- [service orchestration at Twitter][stitch].

The implementation techniques shown in the workshop allows increasing sophistication in the embedded DSL. Untyped interpreters are the baseline. Moving to GADTs (generalised algebraic datatypes) allows us to reuse Scala's type system and remove a whole pile of boilerplate around checking tags. The free applicative and free monad allow us to reuse all of Scala except for small parts of our code where we can insert bits of our DSL. The free structures also allow us to compose together DSLs and their interpreters.

If you'd like more detailed material, it's going into our book [Essential Interpreters][ei] (currently very much under construction).

[workshop-code]: https://github.com/underscoreio/scalax15-interpreters
[ei-code]: https://github.com/underscoreio/essential-interpreters-code
[monadic-io]: http://underscore.io/blog/posts/2015/04/28/monadic-io-laziness-makes-you-free.html
[feature-gating]: http://engineering.instagram.com/posts/496049610561948/flexible-feature-control-at-instagram/
[spark-catalyst]: http://people.csail.mit.edu/matei/papers/2015/sigmod_spark_sql.pdf
[stitch]: https://engineering.twitter.com/university/videos/introducing-stitch
[ei]: http://underscore.io/books/advanced-scala/

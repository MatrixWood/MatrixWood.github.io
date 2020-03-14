---
layout: post
title: Teaching Scala
category: programming
repost: underscore
---

At the beginning of this year I started rewriting [our](http://underscoreconsulting.com/) Scala training material. Like many introductory programming courses ours was structured around language features. This is the most straightforward way to write a tutorial but, particularly in a language like Scala, this structure doesn't work well at all. It's easy to overwhelm the student with details, like the specifics of operator precedence and associativity, that have almost no practical significance for day-to-day programming. All this noise makes it very difficult for the student to pick out the important concepts about how to structure programs written in Scala.

I knew I wanted to rebuild the course around fundamental concepts and use these concepts to motivate the discussion of language features. It's not obvious to me how to write this style of course, but luckily I don't need to reinvent the wheel. Very intelligent people have spent a long time tackling the problem of teaching programming and have documented their experiences in the academic literature. [How to Design Programs](http://htdp.org) is one of the best works I know, which is supported by over 15 years experience teaching across numerous levels, and I modelled our new course after it. The new course has four main themes:

- modelling data;
- processing data;
- sequencing computation; and
- using types.

With this structure some amazing things happen. For one, material that is considered advanced, such as monads and type classes, come easily as they are a natural progression of the themes of the course. Secondly, students learn not just how, but when to use language features. The new material recently had its first outing, and the lessons seemed to transfer from the page to the classroom. Overall I'm very happy with the new approach.

Face-to-face training is great, but there are limitations to the two-day course format. Most obvious is that there is only so much material you can cram into your head in 48 hours. While the realities of business mean the structure of on-site training is unlikely to change, offering online material can support the on-site training and provide a service to those that we can't reach face-to-face. Thus we've decided to flesh out the Core Scala material into a book, and offer it and supporting material for sale online. It will take a while to finish this task but we're confident it will be worth the wait.

If you're interested is hearing about our progress and want to be notified when the course is ready, sign up to the [Underscore mailing list](http://underscoreconsulting.com/blog/posts/2014/03/10/teaching-scala.html) and you'll get regular updates (as well as other interesting news about the Scala world.)

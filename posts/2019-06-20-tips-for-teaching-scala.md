---
title: Tips for Teaching Scala
---

I recently gave a talk on teaching Scala. I first gave the talk at the [Scala in the City][scala-in-the-city] meetup, which was a dry run for the version I gave at [Scala Days][scala-days-talk].  Take a look at [my slides][slides] if this is of interest to you. My talk centered around five tips for teaching. Here I give a quick rundown of the tips and some references for further reading.

<!--more-->

Before getting into the tips a bit of context is useful. When programmers discuss how to teach programming the discussion almost always focuses on the choice of programming language. Someone will say Python, someone else will advocate for Javascript, and there will always be someone who is adamant that [QBasic][qbasic] is the One True Way because that's what they learned with. While I think language is important there are two other really important factors that rarely make it into these conversations: curriculum and pedagogy. Curriculum is what you teach and pedagogy is how you teach. I know from my own experience, and from reading the literature, that these two factors can make a huge difference. For this reason my tips focus on curriculum and pedagogy in equal measure. Let's get on to them.


## Notional Machines

My first tip is to teach some kind of "notional machine", which means a simplified machine model that students can understand programs in terms of. This is most appropriate for beginning learners. The first thought that might come to mind you read this is some kind of [von Neumann machine][von-neumann] with registers, a stack, and heap. In functional programming the notional machine is much simpler: it's algebraic substitution. Substitution is very easy to use (my primary school age children can do it) and allows for compositional reasoning. More on notional machines in [Programming Paradigms and Beyond][programming-paradigms-and-beyond].


## Programming Strategies

Programming strategies are, as far as I know, something unique to how we at [Underscore][underscore] and [Inner Product][inner-product] teach programming. That said, they are heavily inspired by the design recipes in [How to Design Programs][htdp]. The core idea is to allow students to move from problem to solution in a systematic and repeatable way. Programming strategies allow code to written faster with fewer bugs, and by standardizing techniques result in more readable code. They would take many words to explain but the [slides][slides] give some lengthy examples of the main strategies, and my colleague Adam's [slides][adam-slides] describe a larger example.


## More Than Code

This tip is really a reminder that there is more to programming than writing the code. At least two areas need to be taught as well: debugging and tool use. There is a lot of implicit knowledge here, such as interpretering Scala's error messages. These areas can be explicitly taught. [Software Carpentry][software-carpentry], for example, has some lessons on using the shell and Git that are probably very good. However you might not want to take time away from other topics to do this. 

Another way to teach them is implicitly, by demonstrating them as part of other lessons. Live coding is a great way to do this! You're bound to make errors, so you can then demonstrate error recovery and debugging, and you'll naturally be using your tools as you live code. One pro tip: when you hit an error and your brain freezes that is an excellent opportunity to get the students to solve the error. It makes for a better lesson if they're engaged with the problem and it gives you a chance to reboot.


## Shut Up

This tip is something I had to learn the hard way. People need time to think. They need to grapple with problems and fail before they can succeed. When I was a new teacher I was so keen to help that I'd jump in the moment I saw someone struggling. I was so fast they wouldn't get a chance to learn. Whenever there was silence---because a student was figuring something out, for example---I'd rush to fill it with words.

I've learned to slow down a bit, let students move at their own pace, and when I help I try to ask probing questions ("which strategy are you using here?", "what do you think is going wrong?", and so on) rather than addressing the problem directly. It's an unfortunate truth that we can't do the learning for the students. The struggle is real and we must let the student experience that for themselves. (Thanks to Anna Shipman for pointing out the error of my ways!)


## Peer Learning

My final tip is to encourage students to engage in teaching---teaching and learning are in many ways the same thing! For programming tasks, [pair programming][pair-programming] or [mob programming][mob-programming] can work really well. For more general tasks I like to play what I call "the hypothesis game". The game works like this: the teacher asks a question and gets students to vote on an answer. Students should then turn to the person next to them and explain how they arrived at their answer. Students can vote again, to see if they have changed their mind. Finally they should receive the correct answer and an explanation. By explaining their thoughts the student has to try to make a coherent story, which quickly exposes inconsistencies and errors in their mental model. They are then in an ideal state of mind to receive the correct explanation. When working alone the venerable tool of [rubber ducking][rubber-ducking] can be used to simulate an audience.


## Further Resources

The above five tips are far from the final word on teaching programming, so I want to throw in a few more resources that have influenced me.

* Everything [Greg Wilson][greg-wilson] does is great, but if you only read one thing of his make it [Teaching Tech Together][teach-together]. It's short, to the point, and very good. His paper (with [Neil Brown][neil-brown]) ["Ten Quick Tips for Teaching Programming"][10-quick-tips] is also a good read and, with ten tips, has twice the value of this blog post!

* [Visible Learning][visible-learning] collects the work of John Hattie, particularly his large meta-analyses and [rankings][hattie-ranking] of the literature on pedagogical techniques. Although there is some debate on the correctness of his effect size calculations, I believe the overall rankings are in general correct, and they provide for me a very quick way to find the most impactful teaching techniques to study.

* The [PLT][plt] research group provided my first introduction to programming language theory, as distinct from programming, and to teaching programming. I've already mentioned [How to Design Programs][htdp]. [Program by Design][pbd] and [Bootstrap][bootstrap] are other inspirations.


## Conclusions

Teaching programming is a distinct skill from programming, and one that I have only just scratched the surface of. I hope the above helps you become a better teacher, whether you're a senior developer teaching juniors, helping out at a program such as [ScalaBridge][scalabridge], or teaching yourself.


[scala-in-the-city]: https://www.meetup.com/Scala-in-the-City/events/258763565/
[scala-days-talk]: https://portal.klewel.com/watch/webcast/scala-days-2019/talk/6/
[slides]: /downloads/tips-for-teaching-scala.pdf
[qbasic]: https://en.wikipedia.org/wiki/QBasic
[von-neumann]: https://en.wikipedia.org/wiki/Von_Neumann_architecture
[programming-paradigms-and-beyond]: http://cs.brown.edu/~sk/Publications/Papers/Published/kf-prog-paradigms-and-beyond/
[htdp]: http://htdp.org/
[software-carpentry]: https://software-carpentry.org/lessons/
[underscore]: https://underscore.io/
[inner-product]: https://inner-product.com/
[pair-programming]: https://tuple.app/pair-programming-guide
[mob-programming]: https://en.wikipedia.org/wiki/Mob_programming
[rubber-ducking]: https://en.wikipedia.org/wiki/Rubber_duck_debugging
[greg-wilson]: http://third-bit.com/
[teach-together]: http://teachtogether.tech/
[neil-brown]: https://kclpure.kcl.ac.uk/portal/neil.c.c.brown.html
[10-quick-tips]: https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1006023
[visible-learning]: https://visible-learning.org/
[hattie-ranking]: https://visible-learning.org/hattie-ranking-influences-effect-sizes-learning-achievement/
[plt]: https://racket-lang.org/people.html
[pbd]: https://programbydesign.org/
[bootstrap]: https://www.bootstrapworld.org/
[scalabridge]: https://scalabridge.org/
[adam-slides]: https://arosien.github.io/talks/systematic-software.html

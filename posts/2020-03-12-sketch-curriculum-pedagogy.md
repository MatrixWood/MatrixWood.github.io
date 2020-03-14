---
title: A Quick Sketch of Research on Pedagogy and Curriculum for Teaching Programming
---

In preparation for a talk at [NEScala][nescala] I wrote this quick survey on the research on curriculum and pedagogy for teaching programming (curriculum means what we teach, and pedagogy is how we teach it). My goals are:

- give a framework for thinking about curriculum and pedagogy; 
- give some specific tips for teaching programming;
- point out where I believe more research is needed.

My survey is based on the published literature, but it's not a comprehensive document. I'm not an academic and I don't have the time (sadly) to read all the relevant literature. However I've linked to papers I found interesting or important so you can explore further if something grabs your interest. There is a lot of interesting research out there and the tiny bit I've read has made my teaching much better.

<!--more-->

## What Do We Know About Teaching?

We know a lot about teaching, which has been studied for a very long time. To handle this vast literature we can turn to literature surveys. Perhaps the largest surveys are those by [John Hattie][hattie] which aggregates some 1200 meta-surveys which in total aggregate results over some 300 million students. Hattie ranks different techniques according to effect size. There have been various criticisms of Hattie's rankings but I don't believe these criticisms effect the general results, so by looking at the higher ranked methods we can get some good ideas as to effective techniques.

Here are the top ten techniques according to the 2018 survey:

 1. Collective teacher efficacy
 2. Self-reported grades
 3. Teacher estimates of achievement
 4. Cognitive task analysis
 5. Response to intervention
 6. Piagetian programs
 7. Jigsaw method
 8. Conceptual change programs
 9. Prior ability
 10. Strategy to integrate with prior knowledge

There are 122 techniques with effect size at or higher than the average effect size of 0.4. Two questions come to my mind from looking over the list:

 1. Which specific techniques have high effect size and are applicable to teaching programming in my context?
 2. What are the general priniciples behind the effective techniques?

We'll get onto the second question in a bit, when we look at techniques specific to teaching programming. For now let's look at what effective methods have in common. I see a few broad themes:

 - Explicating and correcting students' knowledge (e.g. #8 Conceptual change programs, #18 Summarization, any technique using discussion). It's clear that for learning to take place students need to actively engage with their knowledge. This could be writing notes, completing formative assessment, or writing programs. It's my experience (and I'm sure the experience of many teachers) that students will believe they know something after you have explained it to them but when they come to actually use their knowledge they discover many holes in it. Anything that requires students to engage with their knowledge will show these holes and give them the opportunity to fix them.

 - Learning with other students (e.g. #7 Jigsaw method, #15 Classroom discussion, #27 Reciprocal teaching). There are many effective strategies that involve learning with other students. It's possible that the effectiveness of these methods isn't due to the social aspect, but because discussion and so on forces them to actively engage with their knowledge, but I believe learning with a social component is more effective than learning alone.

 - Explicit learning strategies (meta-cognition; e.g. #4 Cognitive task analysis, #14 Transfer strategies, #17 Deliberate practice). There is a lot of implicit knowledge in programming, as in other domains. For example, strategies for moving from problem statement to working code is usually left implicit. Making this knowledge explicit seems to help learning.

Now we've seen some general principles let's look specifically at programming.


## What Do We Know About Teaching Programming?

We know a lot less about teaching programming than we do about teaching in general, but we still know a fair bit! The general result seems to be that what we know about learning and teaching transfers to programming. For example:

- Peer learning (learning with other students) in various forms works well. There are many different approaches. Some examples include:
    - Pair programming. This is a good practice to use: students get some exposure to a way of working they may encounter in industry, and mentors may already be experienced in pair programming and hence less training is required.
    - [Peer instruction][peer-instruction] is a structured method of discussion suitable for teaching that predominately uses lectures.
    - [Structured peer learning][structured-peer-learning] is a techinque that uses students teachers.

- There are many techniques for explicating knowledge in programming. Programming assigments are perhaps the most obvious way to do this, but there are other techniques. We can ask students to trace through programs (e.g. [tracing recursion][tracing-recursion]) or ask them to solve ["Parsons problems"][parsons-problems]. All of these methods help students to develop a mental model of how programs work (a so-called "notional machine" that describes some abstract machine that executes the code). Working away from code allows the exercises to focus on particular issues that may be obscured by code, particularly when students are not yet proficient with the syntax.

- [Programming strategies][programming-strategies] and [design recipes][htdp] are examples of explicit learning strategies. Giving students explicit strategies allows them to reason about program construction and this seems to be helpful. However this is a fairly new area of study and I think there is much more that can be done here.

There are a lot of choices we can make about a curriculum. Usually the choices are dictated in some part by what is needed to get a job (in a bootcamp) or to support later courses (in a University). We have a bit more freedom at [ScalaBridge London][scalabridge-london] so what I've done is basically walking the line between what I find fun and what I think is best for the students. Briefly:

- We teach functional programming. Substitution is our computing model of choice.
  
- We focus mostly on problem solving and Scala language features. We teach the FP subset of Scala except implicits, and strategies such as structural recursion. We have much less emphasis on producing large systems and preparing students for employment. This is perhaps not a good thing, given most of our students are interested in jobs as programmers.

One feature of our curriculum is introducing programming by creating pictures. This approach is known as "media computation" (see, for example, [this paper][motivation] and [this paper][creative-computation]) in the literature. The evidence suggests this helps increase diversity. Our students certainly seem to find it enjoyable.

Finally, we should note there is much more to programming than just programming. For example, there is tool use (git, editor or IDE, and so on), workflow, debugging, testing, and so on. We don't teach this explicitly. Our approach is to teach parts of this implicitly by having experienced developers work with small groups of students. Some parts (e.g. testing) we don't teach at all.

Bootcamps also offer interview training. Possibly we should do the same.


## What Holes Are There in Our Knowledge?

In my opinion there are several gaps:

 - We don't know enough about teaching and learning functional programming.

 - We don't know much about post-college learners, such as bootcamp or ScalaBridge attendees, though the [research literature][bootcamp] is growing.

I have two beliefs about functional programing:

- it has a much simpler model than alternatives; and
 
- it is amenable to systematic program construction.

Substitution (more formally, "the substitution model of evaluation") is how we reason about functional programs. It is simple to use---we learned how to do this in grade school---and this same simplicity is why experienced FP developers like using it. The same simplicity applies to learning FP. The model is easy to work with, and a minimal FP language (something like Scheme, for example) can be very small.

Functional programming has an underlying mathematical model that makes it, for example, possible to generate code from type declarations. These same strategies can be used by humans to generate code, and you are a (typed) functional programmer you have possibly had the experience where the code just about writes itself once the types are in place. We can (and do!) teach these strategies explicitly, drawing inspiration from [How to Design Programs][htdp]. There are other strategies (e.g. the connection between free structures and Church encodings) that are outside the scope of ScalaBridge but we found important in our commercial work.

Finally we don't know much about the needs of the people who attend ScalaBridge. My suspicion is that ScalaBridge attendees are very similar to bootcamp attendees, which suggests that we should consider doing some of the things that bootcamps do (interview preparation and portfolio development, for example.)


## References

As well as the references embedded in the post there are a few papers that I found to be good overviews:

- [Programming Paradigms and Beyond][paradigms] is a nice overview of the concept of a notional machine, how it is useful for teaching, and raises some open issues in programming education.

- [Computing Education: An Overview of Research in the Field][computing-education] is a recent (2016) paper that does exactly what the title claims, with a focus on high school education.

Finally, if you are interested in exploring the literature here a few tips. Firstly, the computer science education field is pretty terrible in general about releasing their papers in an open way. Most of them are locked behind various paywalls. There are two strategies to combat this: searching the wonderful world wide web for a PDF in the wild or contacting the authors directly. Most academics will be delighted (and somewhat shocked) to have interest in their research and will happily send you a PDF. [Google Scholar](https://scholar.google.com/) is the best search engine I know of for papers. It will also link you to papers that cite the papers you're looking at, and the citations from the current paper. This is a great way to quickly explore the literature.

[nescala]: https://nescala.io/
[hattie]: https://visible-learning.org/
[motivation]: http://andreaforte.net/ForteGuzdialMotivation.pdf
[structured-peer-learning]: https://arxiv.org/abs/1703.04174
[peer-instruction]: https://files.software-carpentry.org/training-course/2012/08/porter-halving-fail-peer-instruction-2013.pdf
[programming-strategies]: https://arxiv.org/pdf/1911.00046.pdf
[htdp]: https://htdp.org/
[tracing-recursion]: http://cs.brown.edu/~sk/Publications/Papers/Published/tfk-eval-trace-rec-subst-nm/paper.pdf
[parsons-problems]: https://www.researchgate.net/profile/Petri_Ihantola/publication/230859647_How_Do_Students_Solve_Parsons_Programming_Problems_-_An_Analysis_of_Interaction_Traces/links/00b49532c151a61b9f000000.pdf
[creative-computation]: https://cs.brynmawr.edu/~dxu/xu257SIGCSE2018.pdf
[bootcamp]: https://par.nsf.gov/servlets/purl/10107751
[scalabridge-london]: https://scalabridgelondon.org/
[paradigms]: http://cs.brown.edu/~sk/Publications/Papers/Published/kf-prog-paradigms-and-beyond/
[computing-education]: https://royalsociety.org/-/media/policy/projects/computing-education/literature-review-overview-research-field.pdf

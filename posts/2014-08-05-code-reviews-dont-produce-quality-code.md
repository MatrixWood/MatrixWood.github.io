---
layout: post
title: Code Reviews Don't Produce Quality Code
category: programming
repost: underscore
---

> We cannot rely on mass inspection to improve quality, though there are times when 100 percent inspection is necessary. As Harold S. Dodge said many years ago, 'You cannot inspect quality into a product.' The quality is there or it isn't by the time it's inspected. [W. Edwards Deming](http://en.wikipedia.org/wiki/W._Edwards_Deming)

Code reviews are a great tool for polishing good code into great code, but they aren't going to turn bad code to good.

At one point in my life I studied [control systems](http://en.wikipedia.org/wiki/Control_system). I have forgotten most of the maths but the general model has stuck with me and I find it a useful lens through which to view software development. Though the formal models of control theory don't capture all that is important about software development, I still find a lot of insight in looking at a system in terms of its control signals and feedback loops.

The most important rule of feedback loops is that you need one if you want to control your system. In software this means that if you never check that what you're producing is useful, which can happen in some projects that are "too big to fail", you are guaranteed to produce garbage code.

The second rule is that not all feedback loops are equal. Even Waterfall has feedback. It just happens too late. The lag time of a feedback loop is very important. The shorter the feedback cycle, the fewer problems there can be, and therefore the cheaper they are to fix. Acceptance testing, pair programming, test driven development, and code reviews are all example techniques you can use to implement a feedback loop, and they all have different cycle times and costs. Pair programming offers immediate feedback but many find it difficult to implement. Acceptance testing usually happens at the end of an iteration. It is often easier to run than pair programmin, but it can invalidate a whole iteration's worth of work. The important point here is that you need to choose the right feedback loop. Techniques differ in cost and speed.

This takes us to code reviews. We often do code reviews for organisations adopting Scala. The usual process is to review code submitted as pull requests. A pull request should be a complete feature. We typically have a single reviewer for a team of 4 or so developers. Code reviews are relatively cheap but they have a relatively large lag from the time the code is written to the time it is inspected.

Code reviews work fantastically well if the code is of a reasonably high quality, and we're looking to add a final level of polish. In this case the changes are of limited scope and can be easily intergrated in the code.

Code reviews fail when the code is of poor quality. In this case bringing the code up to standard may require redesigning the entire feature, essentially throwing away all the work that has been done to date.

I agree with Deming (one of the founders of the Total Quality movement, which in turn inspired agile). You can't inspect in quality -- the lag time is too high. If you have developers who aren't producing good code you need to fix that at the source, by using pair programming, training, and mentoring. If they are producing good code, however, then code reviews can be a very useful tool.

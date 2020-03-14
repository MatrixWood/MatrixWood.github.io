---
layout: post
title: "Strata + Hadoop World London 2015 In Review"
author: Noel Welsh
category: data
repost: underscore
---

I spent last week at the rather ungainly titled [Strata + Hadoop World London][strata-london-2015], O'Reilly's big data conference. I've attended the London event since it started in [2012][strata-london-2012] and it's been interesting to observe how the conference has changed over time.

If I was to summarise the biggest difference from 2013, when I was last at Strata, it is that the field is growing up. The vendor hall was absolutely packed, with traditional enterprise vendors, like Cisco and IBM, making an appearance. The biggest presence, however, belonged to Cloudera. They were certainly putting their $1.2B to use, with the biggest booth in the best location, and sponsorship of everything in sight.

While most vendors were focused on infrastructure, there were companies like Mathworks present that point to a slow shift from storing data to deriving value from it. Whilst there were plenty of talks about how companies are still building their data pipeline it's good to see that many are past that stage and now figuring out how to derive value from their data (which is a much more interesting problem to me).

Another big change was the presence of [Spark][spark], which was everywhere. The Spark sessions I went to were rammed full, and I talked to a number of people coming from a Python or R background who were interested in learning Scala because of Spark. Naturally this is of interest to Underscore.

I missed the first half of the conference because I was still preparing [my slides][whats-there-to-know]. Unlike my previous talks, which were heavy on technical detail, this time I time I went for a higher level discussion of some of the (rarely mentioned) issues that arrive in A/B testing[^differences].

My favourite keynote was "Is Privacy Becoming a Luxury Good?" by [Julia Angwin][julia-angwin]. She made some great points about digital privacy. Rather than me summarizing it here, [go and watch it][strata-london-2015]. It's short and to the point.

I liked how the [Accenture][accenture] team used live demonstrations to sell the power of big data. It's a modern take on the old story-telling maxim "show, don't tell" and continues to be effective.

Mikio Braun's talk on [scalable machine learning][mikio] hit all the right buttons for me. He tells me it's extracted from a semester long course he is teaching. I'm hoping the course notes will find their way online.

The [talk by IDEO][ideo] impressed me with the depth they apply to their product development process. They clearly have a lot of tools in the box. This is a talk that left me wanting more depth -- a lot more depth -- on using data to inform the design process.

Strata was also my introduction to [Flink][flink]. It seems similar to Spark, but is build from the start as a streaming system. It's an interesting project but I'm not sure at this stage it provides enough benefit over Spark to win market share.

For a while I've wanted to move back into the data world after focusing on general software development for a while. Attending Strata definitely strengthened this desire. As a result I've recently started working an internal data project. Watch this space for more information, and commercial offering in the big data space in due course.

[strata-london-2015]: http://strataconf.com/big-data-conference-uk-2015
[whats-there-to-know]: http://noelwelsh.com/downloads/what-to-know-about-ab.pdf
[strata-london-2012]: {% post_url 2012-10-01-strata-slides %}
[spark]: https://spark.apache.org/
[julia-angwin]: http://juliaangwin.com/
[accenture]: http://strataconf.com/big-data-conference-uk-2015/public/schedule/detail/39967
[mikio]: http://strataconf.com/big-data-conference-uk-2015/public/schedule/detail/40341
[ideo]: http://strataconf.com/big-data-conference-uk-2015/public/schedule/detail/42851
[flink]: http://flink.apache.org/

[^differences]: My original talk proposal did have a lot of technical detail, but I ended up with only a twenty minute slot. I thought it would be better to avoid this detail given the time constraint, and I in-fact think higher-level talks are better received at a conference like Strata.

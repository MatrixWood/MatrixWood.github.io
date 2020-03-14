---
title: Benchmarking Web Services using GraalVM Native Image
---

In a [previous] post I discussed the steps needed to compile a [http4s] web service to an executable with [GraalVM Native Image.][native-image]. It's natural to ask what tradeoffs are made by going this route instead of using the JVM. In this post I present several benchmark results I obtained comparing the Native Image executable to the running on the JVM. Read on to find out which comes out on top!

<!--more-->

My goal was to compare executables created with GraalVM Native Image with the same code running on the Java 11 JVM with the GraalVM JIT compiler enabled. 

Before we benchmark we need to decide what we are measuring. The nature of the application determines the performance goal we should optimize for. Two main cases arise in serverless computing:

- infrequently called services, which prioritize startup time and memory consumption; and
- frequently called services, which prioritize long-term performance.

I investigated both cases.

A quick note about my results. My benchmarking environment (also known as my laptop) doesn't allow me to control for many sources of noise that can affect performance measurements.  As a result only large performance differences should be considered valid; small differences might be due to factors I didn't control for. I haven't run any statistical tests on my data, partly because I wasn't able to collect suitable data in some cases and partly because I wasn't interested in the small differences I would need statistical tests to confirm.


## Overview 

My main results are:

- Native Image has orders of magnitude faster startup time and lower memory consumption than the JVM;
- a warmed up JVM has faster response time than Native Image except in the tails, where the JVM response time is much higher.

I also have some additional results from further experiments I ran.

- It seems the 10000 requests is sufficient to warm up the JVM for peak performance.
- The tail response times of the JVM seemed to really get out of hand on some runs. I'm not sure what caused this. My guess is garbage collection pauses, or perhaps interference from something else on my system, but I haven't investigated. Native Image responses in the tail seemed to be much more stable.
- Attempting very simple tuning of the JVM didn't yield noticable results.


## Startup Time and Memory Consumption

Startup time is one of the most important benchmarks for serverless endpoints that are called infrequently. An infrequently used endpoint will not be already running, so the user must wait for it to start before they will receive a response. It is well established that response time is inversely correlated with user satisfaction so we must care about this for any system that interacts with users. 

Memory consumption is another major consideration as all major cloud providers also charge for RAM, usually in 256MB chunks.

To investigate these issues ran an experiment that:

- started a server using the Native Image and the JVM;
- measured the time it took for the server to respond to its first HTTP request; and
- measured memory consumption (resident set size) after this response.

I repeated this process 100 times for each of Native Image and the JVM. The complete process is given by [this shell script][startup-benchmark] (this is reproducible research!) Here's a graph of time to first response.

![GraalVM vs Native Image Time to First Response](/images/benchmarking-graal-native-image/startup-time.png)

Here's memory consumption after the first response.

![GraalVM vs Native Image Resident Set Size after First Response](/images/benchmarking-graal-native-image/resident-set-size.png)

I'm not confident that my experimental setup will be able to distinguish millisecond differences but this doesn't matter because **Native Image is orders of magnitude faster to start and uses orders of magnitude less memory** than the JVM. 


## Response Time Under Sustained Load

Native Image is faster at starting up, but what about response times under load? I expected the JVM would eventually outperform Native Image if the JIT compiler was given enough data to work with. To test this I used the [ab] load testing tool to send requests to each server. I sent requests in batches of 10000 with a maximum concurrency of 50. The full experimental setup is given in [this script][sustained-benchmark]. The graph below shows the zero to 99-percentile response times for Native Image, the cold JVM (the first 10000 requests) and a warm JVM (after 70000 requests).

![0 to 99 percentile response times measured over 10000 requests for Native Image, cold JVM, and warm JVM](/images/benchmarking-graal-native-image/lower-sustained-response-time.png)

This graph shows the 95 to 100-percentile respone times for the same setup.

![95 to 100 percentile response times over 10000 requests for Native Image, cold JVM, and warm JVM](/images/benchmarking-graal-native-image/upper-sustained-response-time.png)

There are a few conclusions I draw from these results:

- Native Image continues to outperform the JVM until the JVM is warm;
- a warm JVM has a modest response time improvement over Native Image; except
- the tail response times (roughly 99th-percentile) for the JVM are much worse than Native Image.

In summary Native Image has much more stable performance than the JVM. It's performance from start is better and it is less affected by large pauses in the tail of the response time distribution.


## JVM Performance Over Time and JVM Tuning

The results from the previous experiment intrigued me. I decided to investigate two questions:

1. How many requests does it take to warm up the JVM?
2. Can I improve the tail performance of the JVM with some simple tuning?

To answer the first question I recorded percentile response times using ab with the same setup as before, but this time for each block of 10000 requests from the start to 80000 total requests. The results are graphed below. I only graphed up to the 95th percentile response time as I didn't want the tails to obscure the rest of the data.

![0 to 95 percentile response times over batches of 10000 requests](/images/benchmarking-graal-native-image/warming-sustained-response-time.png)

The results show no large performance increase after 10000 requests. I feel confident that the JVM is warmed up after the first batch of 10000 requests, but I don't know if I could use fewer requests to achieve the same result. I considered investigating this but decided this was going too far from what I set out to do. It only takes a few seconds to warm up the JVM with my current setup so this isn't a big issue for me.

To answer the second questions I tried two different things:

- setting a 200ms pause time goal for the [G1GC][g1gc] garbage collector; and
- changing some compiler settings that Chris Thalinger used to optimize GraalVM services in his [ScalaDays talk][chris].

Complete settings are given in [the shell script][jvm-tuning-benchmark].

Here are the results.

![0 to 99 percentile response times for various settings of JVM tuning options](/images/benchmarking-graal-native-image/lower-tuned-response-time.png)

![95 to 100 percentile response times for various settings of JVM tuning options](/images/benchmarking-graal-native-image/upper-tuned-response-time.png)

Although there appears to be a small difference in response time between the various JVM settings I don't think my experimental setup is sensitive enough to reliably pick up these small effects.


## Conclusions and Open Questions

Let's have a quick recap. The result that is best supported by the evidence is that the **executables created by GraalVM Native Image start up much faster and use much less memory than the JVM**. My experiments have also shown that **Native Image has faster responses time than a cold JVM** but **a warm JVM has faster response times than Native Image, except above the 98th percentile**. My conclusion is that **Native Image executables are preferred over the JVM** for serverless applications, particularly those where startup time is important or they are IO-bound not CPU-bound.

I also showed that **10000 requests are sufficient to warm up the JVM** in my case, but I wasn't able to show any improvements from simple tuning of the JVM.

There are several open questions. I'm most curious about what is causing the long tail response times I saw on the JVM. The JVM provides plenty of tools to investgate this. My suspicion is it is garbage collection pauses. Using the `-XX:+PrintGC` flag would be a simple way to investigate this hypothesis. Improving the pause times, assuming it is GC, is more challenging. My first step here would be trying the [ZGC][zgc] garbage collector, which is designed to produce pause times less than 10ms. ZGC is not currently available for MacOS, but is available on Linux.

It is an open question how far my results generalize to other applications. I am reasonably confident that short-lived web services---the typical serverless application---will behave in a similar way to my benchmarks. My expectation is that CPU-bound tasks will benefit from additional optimizations in the JVM and that memory-intensive applications will benefit from the more advanced garbage collectors the JVM brings.

Finally, everything I've done---the code, the experiments, and the analysis---is available in [the repository][http4s-native-image] so you can try my experiments with your code or machines. Let me know if you do! I'm very curious to hear how the results translate to different domains.

[http4s]: https://http4s.org/
[previous]: /posts/2020-02-06-serverless-scala-services.html
[g1gc]: https://www.oracle.com/technical-resources/articles/java/g1gc.html
[chris]: https://www.youtube.com/watch?v=ldk8CL0fygE
[ab]: https://httpd.apache.org/docs/2.4/programs/ab.html
[zgc]: https://wiki.openjdk.java.net/display/zgc/Main
[native-image]: https://www.graalvm.org/docs/reference-manual/native-image/
[startup-benchmark]: https://github.com/inner-product/http4s-native-image/blob/master/startup-benchmark.sh
[sustained-benchmark]: https://github.com/inner-product/http4s-native-image/blob/master/sustained-benchmark.sh
[jvm-tuning-benchmark]: https://github.com/inner-product/http4s-native-image/blob/master/jvm-tuning-benchmark.sh
[http4s-native-image]: https://github.com/inner-product/http4s-native-image/

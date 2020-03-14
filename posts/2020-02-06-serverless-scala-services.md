---
title: Building Serverless Scala Services with GraalVM Native Image
---

A recent project has involved serverless web services in Scala, which led me to investigate using GraalVM's Native Image tool to create native executables. In this blog post I describe the steps necessary to build a native executable from a simple [http4s] web service. There is also [complete code][code] accompanying this example.

<!--more-->

## Why Native Image?

Before going into the details it's worth going over a bit of background. [GraalVM][graalvm] is a new Java virtual machine and associated tools that brings increased performance compared to the older Hotspot VM. [Native Image][native-image] is one of the most interesting tools that comes with GraalVM. It compiles JVM bytecode into native executables. This promises reduced startup time and decreased memory consumption compared to running on a full JVM. This is particularly attractive for serverless computing as startup time is an issue and billing often increases with memory consumption.


## Overview of the Process

Building an executable with Native Image is not hard but there is a bit of setup involved the first time. The main steps are:

1. Install GraalVM locally (this is optional, but it is faster to test this way);
2. Writing code (the fun part!);
3. one-off configuration so that GraalVM can handle uses of reflection and `Unsafe`;
4. creating a Docker image to build the executable (optional if you are developing on the same platform you're deploying to); and
5. creating a Docker image to deploy the executable (again, optional).

Let's look at each in turn.


## Installing GraalVM

Installing GraalVM is our local computer is optional (if we go the Docker route, as we do later in this post) but advised as it is much faster to test things locally before invoking Docker. The part of GraalVM we're interested in, [Native Image][native-image], is an addon to the main distribution. You need to install GraalVM first and then use the tool it provides to add Native Image. I used [sdkman] to install GraalVM, so I can run it alongside Hotspot (I need to continue to use Java 8 to build libraries I maintain.) 

With sdkman installed the magic to install GraalVM is

```sh
# Show available JVMs
sdk list java
# Install GraalVM 19.3.1 running on JDK 11
sdk install java 19.3.1.r11-grl 
```

Once you have GraalVM adding Native Image is just a matter of running

```sh
gu install native-image
```

Test everything is installed by running `native-image --version`. You should see output similar to `GraalVM Version 19.3.1 CE`.


## Writing the Code

The next step is to write the code that we'll compile to a native executable. For my case I wrote a [very simple web service][code] using [http4s] and Scala 2.13.

From this code we want to generate a JAR file that Native Image will then compile to an executable. In my case I used the [sbt-assembly] sbt plugin to generate a single fat JAR. Running the `assembly` task in sbt produces a single JAR that I can pass to Native Image with the following command:

```sh
native-image --verbose -jar target/scala-2.13/http4s-native-image-assembly-0.1.0-SNAPSHOT.jar ping-server
```

The `--verbose` flag is not strictly needed but I found it useful for debugging. If you try this using my code it should just work and produce an executable. If you try on your own code, however, you will likely run into some errors because Native Image does not support all the features of the JVM. In the next section I discuss the problems I encoutered and how I fixed them.


## GraalVM Configuration

This step is perhaps the most important. Native Image has various [limitations] which we must work around. The lack of reflection is likely to be an issue for any sizable application. Scala code does not make much use of reflection. Unfortunately we often rely on Java infrastructure that does use reflection. This is the case for http4s, which relys on a Java logging library that uses reflection. For Scala 2.13 there is also an issue with its use of `Unsafe`.

To use reflection with Native Image we must tell Native Image ahead of time which classes will be reflected so it can generate the appropriate data. There are various ways to do this but the most modular is to create files called `reflect-config.json` in `src/main/resources/META-INF/native-image/package-name/project-name` replacing `package-name` and `project-name` with appropriate names. [This is my example.][reflect-config] The format is reasonably self explanatory. If we do this we can package our configuration with our code and Native Image will automatically find it. Otherwise we must provide additional command line arguments telling Native Image where to find our configuration.

We can configure other Native Image settings by creating a file called `native-image.properties` in the same location. This is important to get Scala 2.13 working with Native Image. Scala 2.13 has some uses of `Unsafe`, encapsulated in `scala.runtime.Statics`, that Native Image does not recognise and hence cannot compile. To get around this create a [file][native-image-properties] `src/main/resources/META-INF/native-image/org.scala-lang/scala-lang/native-image.properties` containing

```
Args = --initialize-at-build-time=scala.runtime.Statics$VM
```

This initializes the object `scala.runtime.Statics` at compile-time and copies the object into the native executable's heap, getting around the problem.

We can set other Native Image commands this way, which again allows for an encapsulated and modular build. Here's what I set for `co.innerproduct/ping`, which applies to the code I created.

```
Args = -H:+ReportExceptionStackTraces --allow-incomplete-classpath --no-fallback
```

These parameters, in order, are used to:

- report more informative traces in case of errors;
- allow compilation of code that does not contain all classes that are referenced in the code, which is important for uses of reflection; and
- fail the build instead of generating fallback code when Native Image cannot resolve uses of reflection or other issues.

More information can be in the [Native Image documentation](https://www.graalvm.org/docs/reference-manual/native-image/#image-generation-options) and if you use code that makes heavy use of reflection you might want to use the [Native Image agent](https://github.com/oracle/graal/blob/master/substratevm/CONFIGURE.md) to automatically generate your configuration.

A final issue I ran into was a complaint about Janino. I don't understand what is causing this but adding Janino as a dependency to my project (and hence including it in my fat JAR) solved the issue.

With all the above in place the code should compile and, several hundreds of thousands of milliseconds later, we'll have a owkring executable. In my case I can connect to port 8080 with my web browser and see the server is indeed working.


## Cross-Building Using Docker

I'm building on a Mac laptop but I want to deploy on Linux. Native Image does not support cross-building, so to build a Linux executable I must run Native Image within a Docker container.

Oracle provides Docker images with GraalVM, but not with Native Image. It's a simple matter to create an image with Native Image using the Oracle images as a base. Here's the `Dockerfile`

```
ARG GRAAL_VERSION=19.3.1-java11
FROM oracle/graalvm-ce:${GRAAL_VERSION}
WORKDIR /opt/native-image
RUN gu install native-image
ENTRYPOINT ["native-image"]
```

I can build this image using the command, from within the directory that contains the `Dockerfile` (which is `docker` in my code; this directory contains nothing else to keep the resulting image as small as possible.)

```sh
docker build -t inner-product/graalvm-native-image .
```

Now I can use the image I just made to cross-build a Linux executable. I created a sbt task to do this, so that my task could depend on the local of the fat JAR built by `sbt-assembly`. Here's the code for this task

```scala
lazy val nativeImage =
  taskKey[File]("Build a standalone executable using GraalVM Native Image")

nativeImage := {
  import sbt.Keys.streams
  val assemblyFatJar = assembly.value
  val assemblyFatJarPath = assemblyFatJar.getParent()
  val assemblyFatJarName = assemblyFatJar.getName()
  val outputPath = (baseDirectory.value / "out").getAbsolutePath()
  val outputName = "ping-server"
  val nativeImageDocker = "inner-product/graalvm-native-image"

  val cmd = s"""docker run
     | --volume ${assemblyFatJarPath}:/opt/assembly
     | --volume ${outputPath}:/opt/native-image
     | ${nativeImageDocker}
     | --static
     | -jar /opt/assembly/${assemblyFatJarName}
     | ${outputName}""".stripMargin.filter(_ != '\n')

  val log = streams.value.log
  log.info(s"Building native image from ${assemblyFatJarName}")
  log.debug(cmd)
  val result = (cmd.!(log))

  if (result == 0) file(s"${outputPath}/${outputName}")
  else {
    log.error(s"Native image command failed:\n ${cmd}")
    throw new Exception("Native image command failed")
  }
}
```

Now I can simply run the `nativeImage` task from within sbt and, after a substantial pause, I will have a Linux executable called `ping`.

There's really only one thing to note about this command: I passed the `--static` parameter to Native Image. This creates a statically linked executable that has no dependencies on external libraries. This can be important when deploying as discussed in the next section.


## Creating a Deployment Docker Image

The final step is to create a Docker image in which I can deploy my code. Because I built a statically linked executable I don't need very much external support, so I can create a very small image. Alpine Linux is a distribution created specifically for the use case of building small Docker images. I can create my Docker image using Alpine Linux and the executable I built earlier with the following Dockerfile.

```
FROM alpine:3.11.3
COPY ping-server /opt/ping-server/ping-server
RUN chmod +x /opt/ping-server/ping-server
EXPOSE 8080
ENTRYPOINT ["/opt/ping-server/ping-server"]
```

We build this image with the following command, assuming the Dockerfile is in the current directory which also contains the executable we previously built.

```sh
docker build -t inner-product/ping-server .
```

We can run the resulting image with using the `docker run` command.


```sh
docker run -d -p 8080:8080/tcp inner-product/ping-server
```

Now visit `http://localhost:8080/ping/hello` and you should see a result!

Finally stop the Docker using the container id printed when you ran it above.

```sh
docker stop <container-id>
```

## Conclusions

We've seen that creating an executable from Scala code using GraalVM Native Image is quite straightforward. There is a bit of one-off cost to get everything setup, but you can copy the work I've done and, if you're just using http4s and other Typelevel projects, it is unlikely you'll have to do additional configuration. Compilation is not fast, but this is something you could offload to a CI/CD server. The resulting executable is easy to deploy.

One of the attractions of native executables is faster startup and reduced memory consumption compared to the standard JVM. This, along with deployment, is something I'll look at in the next in this series.


## Acknowledgements

I relied on lots of different resources to get this all working. I've linked to documentation above where it is relevant but there were a few other sources I used:

- [This Scala bug report](https://github.com/scala/bug/issues/11634) contained the solution to getting Scala 2.13 working with GraalVM Native Image.
- [This blog post](https://blog.softwaremill.com/small-fast-docker-images-using-graalvms-native-image-99c0bc92e70b) from SoftwareMill was a useful reference for the Docker part of the puzzle.

Thanks Dale and Adam!


[limitations]: https://github.com/oracle/graal/blob/master/substratevm/LIMITATIONS.md
[sdkman]: https://sdkman.io/
[native-image]: https://www.graalvm.org/docs/reference-manual/native-image/
[http4s]: https://http4s.org/
[code]: https://github.com/inner-product/http4s-native-image
[graalvm]: https://www.graalvm.org/
[sbt-assembly]: https://github.com/sbt/sbt-assembly
[reflect-config]: https://github.com/inner-product/http4s-native-image/blob/master/src/main/resources/META-INF/native-image/co.innerproduct/ping/reflect-config.json
[native-image-properties]: https://github.com/inner-product/http4s-native-image/blob/master/src/main/resources/META-INF/native-image/org.scala-lang/scala-lang/native-image.properties

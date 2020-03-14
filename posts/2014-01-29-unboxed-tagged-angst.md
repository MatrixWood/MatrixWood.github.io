---
layout: post
category: programming
title: Unboxed Tagged Angst
repost: underscore
---

Type class based serialization is now standard in Scala JSON libaries such as [Play JSON](http://www.playframework.com/documentation/2.2.x/ScalaJsonCombinators). All our web applications these days are designed as JSON APIs, with the UI being just an API client. We usually find we want a few different serialization formats. Here are two examples that came up recently: logged-in users can see more information than anonymous users; and, as we're using Mongo, we want a serialization format for the database that includes more information than other clients can see. Thus we need to control which type class is used for serialization at each point.

Manually importing the correct type class into scope is one approach to controlling type class visibility. This is a fantastic way to introduce bugs, as nothing in the type system will fail if we import the wrong type class.

As better approach, and the one we've been using, is to tag the data and the type classes. I think some code helps at this point.

Let's say we have a basic `User` class

``` scala
case class User(name: String, email: String)
```

In Play we can construct a serializer like so:

``` scala
import play.api.libs.json._

implicit val userWrite: Writes[User] = Json.writes[User]
```

A `Writes[User]` is a type class that can write a `User` as JSON. If we want to write a `User` we can call `Json.toJson(user)` and the usual implicit resolution rules will look for a `Writes` in scope.

Now suppose we don't want to display email addresses to anonymous users. We can define a new `Writes` easily enough.

``` scala
implicit val anonymousUserWrites = new Writes[User] {
  def writes(in: User): JsValue =
    Json.obj("name" -> in.name)
}
```

The question is: how do we make sure this implicit is used at the correct points, in a way that the compiler will complain to us if we get it wrong?

We've followed Scalaz's lead, using [unboxed tagged types](http://etorreborre.blogspot.co.uk/2011/11/practical-uses-for-unboxed-tagged-types.html). They are fairly simple beasts. The constructor `Tag[A, T](a: A)` applies the tag `T` to a value `A`. Tags are just empty traits and a tagged type, written `A @@ T`, is a subtype of `A`. Here's the code:

``` scala
trait Anonymous
def anonymous[A](in: A): A @@ Anonymous = Tag[A, Anonymous](in)
```

Now we just need to tag `anonymousUserWrites`, so it only applies to `User`s tagged `Anonymous`, and we're in business.

``` scala
implicit val anonymousUserWrites = new Writes[User @@ Anonymous] {
  def writes(in: User @@ Anonymous): JsValue =
    Json.obj("name" -> in.name)
}
```

Or so I thought.

I've used tagged types before to control implicit selection, but I recently did my first implementation mixing them with Play JSON. After creating the tags and tagging the values, but not implementing any tagged type classes, I decided to check that this approach would work. It should fail to compile, because no tagged implicits are available. Imagine my surprise when everything did in fact compile! What! The whole point of tagging is to stop things compiling if a tagged implicit is not also available!

I spend a few hours looking into this issue without success, and I began freaking out a bit. What dark corner of Scala's type system had I run into? Was the savoir faire of Play's design beyond my dour comprehension? Would I have to hand in my type-astronaut wings if I couldn't fix this problem? Would Miles ever speak to me again if he found out? Luckily, at this point my wife phoned. The car's battery was flat. She was stuck at work, and I needed to hop on my bike and collect the kids pronto. Inspiration came while pedalling home with 40kg of boys in the trailer behind: contravariance!

Remember that tagged types are subtypes of the original type. The original, untagged, implicit instance was being picked up when we had a tagged value. This could only happen if the untagged instance was considered a subtype of a tagged instance, and that would only happen if `Writes` was contravariant. When I got home I checked the [docs](http://www.playframework.com/documentation/2.2.x/api/scala/index.html#play.api.libs.json.Writes) and found I was correct. I then ripped out all the tagged types and used a different method, but that's another story and will be told another time.

## Lessons Learned

Getting stuck in your head with a problem is often not a good idea, but I find it hard to remember to change context when I get stuck. I enjoy problem solving so when I run into a problem I want to stay at the keyboard and fix it! [Rubber ducking](http://www.c2.com/cgi/wiki?RubberDucking) is the same idea that doesn't require ready access to a bike and kids.

My wife is fond of saying ["when you hear hoofbeats, think of horses not zebras"](http://en.wikipedia.org/wiki/Zebra_%28medicine%29), which means look for the straightforward answer first. When I ran into this problem I started looking for corner cases in the type system, compiler bugs, and other esoterica. The problem involved concepts I already knew, contravariance and implicit resolution, but combined in a way I hadn't seen before. If I had ruled out the basics first I would solved this problem quite quickly.

Finally, subtyping is evil. Or at least probably doesn't carry its weight when one gets into a highly "typeful" programming style. Scala is an interesting place with regards to subtyping. The ease of interoperation and the gentle slope from Java make Scala attractive to many, and here subtyping seems essential and type classes like wild and dangerous constructs. However, as you continue down the Scala road the language you end up using is not the language you started with. What once seemed essential can become an impediment. There is no doubt that Haskell has a cleaner take on typeful programming than Scala, but compatibility with the JVM and Java, both good and bad, is the trade-off that Scala makes.

---
layout: post
title: AngularJS vs React
category: programming
repost: underscore
---

[AngularJS](https://angularjs.org/) and [React](http://facebook.github.io/react/index.html) are two of the most prominent Javascript UI frameworks today. It is natural to ask which is best. Having some experience with both my opinion is that React is superior by a large margin.

I'll get into why in a moment, but first some background so you can understand how I came to this conclusion. I'm not very experienced with either framework. I have written some small projects in AngularJS. I've spent about three days porting part of a medium sized AngularJS project to React, which prompted this writeup. It's possible with more experience of both frameworks I would come to a different conclusion, but for reasons I explain at the end I don't think this will be the case.

With that out of the way, on to my comparison.


## Simplicity

My number one reason for preferring React to AngularJS is its simplicity. This is very apparent when learning the frameworks. AngularJS introduces an explosion of new concepts. There is, at least, *controllers*, *directives*, *factories*, *scopes*, *services*, *transclusion*, the directives library, the module system, and more that I've no doubt forgotten about or haven't encountered yet. Add to that the famously bad documentation and you have a learning curve like a cliff.

In React there are just three important concepts: *components* with *properties* and *state*. Components are just code. There is so little to React that when I started to learn it I could read all the documentation in just one day. The documentation also made sense, a major advantage over AngularJS.


## Composition

In React you compose programs out of components. As components are just code, composition works the way you, as a programmer, would expect. All the usual abstraction mechanisms of functions, objects, and so on, work in the usual way. React is just normal code and using React is just normal programming.

In Angular you compose applications out of directives, which involves programming in both Javascript and HTML, and other stuff which is written in Javascript. As directives span both Javascript and HTML the composition rules are rather complicated. Add to that HTML does not make the basis for a great programming experience. Programming in XML-ish syntax was a bad idea when I used [Ant](http://en.wikipedia.org/wiki/Apache_Ant) 15 years ago and remains a bad idea today. The end result is AngularJS programs tend not to compose as easily as React programs in my experience.


## Architecture

Finally, I find React encourages a better architectural style than AngularJS. Now architecture is up to the programmer, and you can write spaghetti in any language, but nonetheless I believe one-way data binding, as offered by React, leads to clearer programs than AngularJS's two-way data binding. The reason is that data flow is much clearer when data flows in only one direction. You can trace a React program from start to finish. An AngularJS program, on the other hand, is more like a constraint system, with no clear start and end point. It can be very hard to determine what depends on what as an Angular program grows.


## Conclusions

It certainly is possible to climb the learning curve with AngularJS and be productive using it. Is it worth it? My experience suggests it isn't. React can be used to achieve the same things as AngularJS, it's much simpler to use, and it has some nice optimisations that AngularJS doesn't. All up I greatly prefer React to AngularJS.

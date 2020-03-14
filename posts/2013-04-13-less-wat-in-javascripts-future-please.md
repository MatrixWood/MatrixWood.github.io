---
layout: post
title: "Less Wat in Javascript's Future, Please"
description: ""
category: programming
tags: [punditry]
lead: Prominent members of the Javascript community are working on a standard for Promises (or Futures) in Javascript. The proposed design is lacking in fundamental ways, repeating mistakes from Javascript's past. Isn't it time we did better?
---
{% include JB/setup %}

If you're a Javascript developer you've probably viewed the [Wat](https://www.destroyallsoftware.com/talks/wat) talk. If not, go watch it now. It's short, funny, and brilliantly illustrates the foibles of Javascript.

Bashing on Javascript is old. Its shortcomings are well known and I don't want to dwell on them. I do hope we can learn from them, however, and not repeat the same mistakes in the future.

This sequence from the talk illustrates one of the main issues in Javascript:

{% highlight javascript %}
> [] + []
''
> [] + {}
[object Object]
> {} + []
0
> {} + {}
NaN
```

This behaviour has been [explained](http://stackoverflow.com/questions/9032856/what-is-the-explanation-for-these-bizarre-javascript-behaviours-mentioned-in-the/9033306#9033306) on Stackflow, but let's go back a step and ask why would the language even behave like this in the first place?

The fundamental issue here is some rather strange type conversions (the empty string is converted to the number 0, for example.) It's difficult to reverse engineer Brendan's Eich state of mind during that [dark and lonely period in 1995](https://brendaneich.com/2010/07/a-brief-history-of-javascript/) when Javascript was created, but we can take a reasonable guess. Javascript was intended to be the browser language for tying together components which would be implemented in Java, and in this context a slew of type conversions might seem like they'd simplify things.

Unfortunately, it's a false simplicity. While there might be a reasonable explanation for each individual special case, taken together they form an impenetrable mass of complexity.

Sadly the same mistakes continue to be repeated. Take jQuery's [map](http://api.jquery.com/jQuery.map/) function for example. It works in a straight forward way:

{% highlight javascript %}
> $.map([1, 2, 3], function(x) { return x + 1; })
[2, 3, 4]
```

Except if the function returns an array, in which case that array is appended to the array of results! If you want to create an array of arrays you're SOL.

{% highlight js %}
> $.map([1, 2, 3], function(x) { return [x, x + 1]; })
[1, 2, 2, 3, 3, 4] // Not [[1,2], [2,3], [3,4]] as we'd expect!
```

Map and appending (sometimes called `flatMap`) is a different thing to just mapping, and jQuery should provide different functions if it wants to support both. Again, there is a seemingly reasonable explanation for this behaviour, but this focus on the small scale ignores the bigger picture impact of adding yet another special case to a language ecosystem already overburdened with caveats and incompatibilities.

A more recent example is the [Promises](http://promises-aplus.github.io/promises-spec/) specification. Again we have a case of false simplicity: the specification requires only a single method (`then`) but builds a mountain of complexity in the special cases handled by that function.

In the case of Promises it's not too late to change things. [A better way](https://github.com/promises-aplus/promises-spec/issues/94) has been proposed, but has met with resistance. The main argument against seems to be "that's not how we've done it in the past." Javascript's past, as we've seen, is not a reliable guide for design quality.[^monads]

[^monads]: Thankfully, in this case more progressive minds [are exploring alternatives.](https://github.com/promises-aplus/promises-spec/issues/97)

The time when Javascript was primarily used for gluing together a few DOM manipulations has long passed. Javascript programs are big, and Javascript developers are professionals. It's time to do better. Clean and simple APIs are what Javascript needs, not more wat.

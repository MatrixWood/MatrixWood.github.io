---
title: Gatsby is a Virus; Here's the Cure
---

[Gatsby][gatsby] is a Javascript framework for building static sites. It has the pernicious effect of infecting the browser cache, and won't go away until you manually clear the cache or deploy a file that kills it. Here I describe the problem and the solution.

<!--more-->

The herpes virus is a common virus with one very unfortunate ability: your body can never truly gets rid of it. Instead, when the initial infection dies down it goes to live in your ganglia, waiting to reemerge at the least convenient time. Gatsby is similar, though instead of infecting human ganglia it infects browser caches. There it will live indefinitely, causing browsers to display old pages even if you no longer use Gatsby. Luckily, unlike herpes, there is a way to kill Gatsby for good so that an up-to-date site will be displayed.

I created some simple static sites using Gatsby. I wanted to experiment with some different technology, which was fun for a bit, but after a while I decided Gatsby wasn't for me. I shifted my sites to another static site generator and here is where the fun began. I noticed whenever I visited my site the old Gatsby pages were displayed, no matter how often I refreshed. I assumed it was due to caching that would soon expire, so I manually cleared the cache and moved on. The sites aren't heavily trafficed so I didn't notice any issue. However, over sixty days later I opened up one of the sites in a web browser I don't use very often---and the old Gatsby pages displayed! Gatsby had infected the browser cache and *after two months* it still had not expired! One of the static sites is likely to get more traffic in the near future so I knew I had to resolve this.

I couldn't find the issue documented on Gatsby's site, but a bit of searching found some discussion of web workers. I don't fully understand how Gatsby uses web workers but I worked out the cure nonetheless. To kill Gatsby you need to deploy a file called `sw.js` in the root directory which should contain

```javascript
self.addEventListener('install', function(e) {
    self.skipWaiting();
});

self.addEventListener('activate', function(e) {
    self.registration.unregister()
        .then(function() {
            return self.clients.matchAll();
        })
        .then(function(clients) {
            clients.forEach(client => client.navigate(client.url))
        });
});
```

With this in place Gatsby will be killed and your new site will be displayed.

I opened [an issue][issue] about this. I was disappointed with the response. The team don't seem to feel they have any responsibility to even document this issue. Hence I'm documenting the problem and solution here in the hope others will benefit from it.

[gatsby]: https://www.gatsbyjs.org/
[herpes]: https://en.wikipedia.org/wiki/Herpes_simplex_virus
[issue]: https://github.com/gatsbyjs/gatsby/issues/15623#issuecomment-510405199

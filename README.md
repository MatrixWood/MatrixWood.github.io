# My Personal Site

[![Netlify Status](https://api.netlify.com/api/v1/badges/7bdffce3-479c-4446-b146-38185e2b9270/deploy-status)](https://app.netlify.com/sites/matrixwood/deploys)

Written in Hakyll

## Cheatsheet

To build and view

```sh
stack exec site watch
```

To just build

```sh
stack exec site rebuild
```

After making changes to `site.hs`

```sh
stack build
```

To deploy

```sh
netlify deploy --prod -d _site/
```

# My Personal Site

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

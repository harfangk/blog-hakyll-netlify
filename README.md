# Harfang's Perch

This is the repository for my blog at [https://harfangk.dev](https://harfangk.dev). It's built using Hakyll and hosted on Netlify.
Built using sass 1.41.0 on npm transpiled to Javascript from Dart.

## Build

```shell
# When on macOS, install pkgconf to build digest package
$ brew install pkgconf 
$ stack build
$ stack exec blog-hakyll-netlify-exe build
$ stack exec blog-hakyll-netlify-exe watch
```

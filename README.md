# tachyons-to-tailwind

A tool for migrating codebases using tachyons to tailwind. It reads the supplied
source files looking for tachyons classes and replaces them with equivalent
tailwind classes.

## Getting started

For development you will need to install GHC and Cabal (the build tool for
Haskell projects). The easiest way to do this is with
[ghcup](https://www.haskell.org/ghcup/). This should install the latest version
of GHC, along with a compatible version of `cabal-install`.

## Running

Execute the `tachyons-to-tailwind` script at the root of the repo.

```sh
$ ./tachyons-to-tailwind --help
```

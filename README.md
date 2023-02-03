# Stream fusible lists

Faster lists using stream fusion.

The abstract from the original paper by Duncan Coutts and Don Stewart:

> This paper presents an automatic deforestation system, \emph{stream
> fusion}, based on equational transformations, that fuses a wider
> range of functions than existing short-cut fusion systems. In
> particular, stream fusion is able to fuse zips, left folds and
> functions over nested lists, including list comprehensions. A
> distinguishing feature of the framework is its simplicity: by
> transforming list functions to expose their structure, intermediate
> values are eliminated by general purpose compiler optimisations.
>s
> We have reimplemented the Haskell standard List library on top of
> our framework, providing stream fusion for Haskell lists. By
> allowing a wider range of functions to fuse, we see an increase in
> the number of occurrences of fusion in typical Haskell programs. We
> present benchmarks documenting time and space improvements.

Building:

  $ cabal build

Use:

  import Data.List.Stream

and use as you would for normal lists.

This repository is part of a renewed effort to get stream fusion into base.
One of the main thresholds was the ability to fuse the 'concatMap' function.
Now in 2023, the GHC proposal [Higher Order Patterns in Rewrite Rules](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0555-template-patterns.rst) is expected to grant us that ability.
Hence, I am returning to this package to get actual benchmark results.
# Stream fusible lists

Faster lists using stream fusion.

The abstract from the original paper by Duncan Coutts and Don Stewart:

> This paper presents an automatic deforestation system, *stream
> fusion*, based on equational transformations, that fuses a wider
> range of functions than existing short-cut fusion systems. In
> particular, stream fusion is able to fuse zips, left folds and
> functions over nested lists, including list comprehensions. A
> distinguishing feature of the framework is its simplicity: by
> transforming list functions to expose their structure, intermediate
> values are eliminated by general purpose compiler optimisations.
>
> We have reimplemented the Haskell standard List library on top of
> our framework, providing stream fusion for Haskell lists. By
> allowing a wider range of functions to fuse, we see an increase in
> the number of occurrences of fusion in typical Haskell programs. We
> present benchmarks documenting time and space improvements.

## Building

```bash
$ cabal build
```

## Use

```haskell
import Data.List.Stream
```

and use as you would for normal lists.

## Stream Fusion in 2023

This repository is part of a renewed effort to get stream fusion into base.

There have been several new developments since the original work by Duncan and Don.
It seems like 2023 is the year when GHC is finally ripe for their ideas.

In 2017, Luke Maurer, Zena Ariola, Paul Downen, and Simon Peyton Jones wrote the paper [Compiling without Continuations](https://ix.cs.uoregon.edu/~pdownen/publications/pldi17_appendix.pdf) which introduces join points as optimization technique and states:

> Coutts et al. suggested adding a Skip construtor to Step, thus:
> ```haskell
> data Step s a = Done | Yield s a | Skip s
> ```
> Now the stepper function can say to update the state and call
> again, obviating the need for a loop of its own. This makes
> filter fusible, but it complicates everything else! [..]
>
> But with join points, just as with any, Svenningsson’s
> original Skip-less approach fuses just fine! Result: simpler
> code, less of it, and faster to execute. It’s a straight win.

Still, one other important threshold was the ability to fuse the 'concatMap' function.
Now in 2023, the GHC proposal [Higher Order Patterns in Rewrite Rules](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0555-template-patterns.rst) is expected to grant us that ability.

Hence, I am returning to this package to get actual benchmark results after all these improvements.
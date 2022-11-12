# gcl

Project for the Program Semantics &amp; Verification course

## Requirements

* GHC >= 9.0
* Cabal >= 3.6

(Both can be installed via [ghcup](https://www.haskell.org/ghcup/))

## How to run

```sh
# First, compile the project
cabal build

# Usage (The first -- is necessary to disambiguate cabal's arguments from the program's arguments)
cabal run . -- [--unroll-depth K] [--no-simplify] [--no-prune] [--show-stats] [--show-paths] [--show-preds] PATH
cabal run . -- --help

# Examples
cabal run . -- examples/min.gcl
cabal run . -- --show-stats --no-simplify examples/min.gcl
```

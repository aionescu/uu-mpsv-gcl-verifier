# gcl-verifier

[GCL](https://en.wikipedia.org/wiki/Guarded_Command_Language) verification tool based on [predicate transformers](https://en.wikipedia.org/wiki/Predicate_transformer_semantics), developed as part of the Program Semantics & Verification course at Utrecht University.

Developed in collaboration with [Maksymilian Demitraszek](https://github.com/MaksymilianDemitraszek).

## Requirements

* GHC >= 9.0
* Cabal >= 3.6

Both can be installed via [ghcup](https://www.haskell.org/ghcup/).

## Building

To build the tool, simply run `cabal build` in the directory of the repo.

Afterwards, you can run the tool with `cabal run`. See below for usage instructions.

## Usage instructions

The tool's interface is optimized for "real-world" usage. Namely, by default all heuristics are enabled, and the tool prints nothing. It returns an exit code depending on the verification result: 0 if all paths are correct, 1 if there are incorrect paths.

In order to see the paths and the counter-examples, use the `--show-paths` or `--show-preds` flags. `--show-paths` shows the generated program paths, while `--show-preds` shows their WLP formulas. Both can be enabled at once.

Below are more detailed instructions, as well as some examples:

```sh
# The first -- is needed to disambiguate between arguments to cabal and arguments to our tool.
cabal run . -- --help
cabal run . -- [--depth K] [--N N] [--no-prune] [--no-simplify] [--dump-ast] [--show-stats] [--show-paths] [--show-preds] [--mutate] PATH

# Available options:
#   -h,--help                Show help text
#   --depth K                Maximum depth of program paths (default 50)
#   --N N                    Experiment parameter (optional)
#   --no-prune               Disable pruining unfeasible paths
#   --no-simplify            Disable front-end simplifier
#   --dump-ast               Show AST after parsing
#   --show-stats             Show verification statistics
#   --show-paths             Show paths and results
#   --show-preds             Show preds and results
#   --mutate                 Run with mutations
#   PATH                     The source file to verify

# Examples:

# Verify `min.gcl` with N set to 5 and default options
cabal run . -- --N 5 bench/min.gcl

# Verify with different path depth
cabal run . -- --N 5 --depth 80 bench/min.gcl

# Show statistics and WLP formulas, disable all heuristics
cabal run . -- --N 5 --depth 80 --show-stats --show-preds --no-prune --no-simplify bench/min.gcl

# Run with mutations and print "kill-rate"
cabal run . -- --mutate bench/min.gcl
```

## GCL Programs

You can find the GCL programs we used for benchmarking in the [bench](bench) folder, and other programs we experimented with are in the [examples](examples) folder.

## VS Code Extension

We have also provided a VS Code extension for GCL syntax highlighting. You can install it by running the `install-vscode-ext.sh` script, or by copying the `gcl-vscode` folder into your VSCode Extensions folder.

There's also a script for uninstalling the extension: `uninstall-vscode-ext.sh`.

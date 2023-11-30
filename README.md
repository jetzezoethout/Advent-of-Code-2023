# AoC 2023

## Haskell setup

Solutions have been tested with the following toolchain version:

```bash
$ ghcup list -c set
✔✔ ghc   9.4.7
✔✔ cabal 3.8.1.0
✔✔ hls   2.4.0.0
✔✔ stack 2.11.1
✔✔ ghcup 0.1.20.0
```

Another note: all solutions have been tested on a 64-bit system, where `Int` ranges from `-2^63` to `2^63-1`. On other systems, some solutions may not work due to overflows. In this case, replacing `Int` by `Integer` in appropriate places should do the trick.

## Running the solutions

In order to run the solution for a given day, execute:

```bash
cabal run dayxx -- <path to input file>
```

Assuming your input file for `dayxx` is `dayxx/input.txt`, you can run all solutions at once using:

```bash
cabal build
./runSolutions.sh
```

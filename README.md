# Building on developers' intuitions to create effective property-based tests

QuickCheck demo code from a [talk](https://youtu.be/NcJOiQlzlXQ) by
John Hughes.

## Usage

If you're a [Nix](https://nixos.org/nix/) user start by running
`nix-shell`.

Everyone:

```
$ cabal new-configure && cabal new-build && cabal new-test && echo $?
$ cabal new-repl test:tests
> :load Main
> main
> :m Test.QuickCheck
> :load Coin
> quickCheck Coin.prop_Add''
+++ OK, passed 100 tests:
52% normal
48% overflow
```

See [./tests](./tests) for more property-based tests. Comments help
explain the incremental refinements.

## Repro of haskell.nix + GHC plugin

It segfault on GHC 8.10.7.

# Steps

Enter into `nix develop` and run `cabal build`

# Expected

no segfaults (But happens)

NOTE: bug triggered by adding
```haskell
GHC.pluginRecompile = GHC.purePlugin
```

## GHC plugin for wrapping all string literals with  Text type annotation

### Motivation

```haskell
    class AsText a where
      asText :: a -> Bool

    instance AsText String where
      asText _ = False

    instance AsText T.Text where
      asText _ = True

    bar = asText "foo"
```

Fail as ambiguous string literal found, compiler can't decide instance here.

We wanted overloaded strings to be a `Text` by default, so plugin add type annotation

```haskell
    bar = asText ("foo" :: Data.Text.Text)
```

qualified import on Data.Text also would be auto added:

```haskell
    import qualified Data.Text
```

# GHC versions support

Tested with GHC 8.8.4

Code written with other GHC versions in mind, but no tests was performed.

# Copyrights

Development backed by Monadfix OU, and Juspay Technologies Pvt Ltd
Shim.hs was borrowed from `large-records`, and copyrighted by Edsko de Vries and Well Typed.

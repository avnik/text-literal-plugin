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

Tested with GHC 8.8.4

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Text.Literal.Plugin (plugin) where
import Data.Text.Literal.Shims
import Data.Traversable (for)
import Data.Monoid (Any)
import Control.Monad.Writer
import Control.Monad
import qualified Data.Generics.Uniplate.Data as Uniplate

-- | GHC plugin.
plugin :: Plugin
plugin = defaultPlugin
  { pluginRecompile = purePlugin
  , parsedResultAction = aux
  }
  where
    aux ::
         [CommandLineOption]
      -> ModSummary
      -> HsParsedModule -> Hsc HsParsedModule
    aux _opts _summary parsed@HsParsedModule{hpm_module = modl} = do
        modl' <- transformDecls modl
        pure $ parsed { hpm_module = modl' }

transformDecls :: LHsModule -> Hsc LHsModule
transformDecls (L l modl@HsModule {hsmodDecls = decls, hsmodImports}) = do
    (decls', getAny -> needImports) <- runWriterT $ for decls $ transformDecl 

    let modls = if needImports then [importDecl dataTextModule True] else []

    -- We add imports whether or not there were some errors, to avoid spurious
    -- additional errors from ghc about things not in scope.
    pure $ L l $ modl {
        hsmodDecls   = decls'
      , hsmodImports = hsmodImports ++ modls
      }


transformDecl :: LHsDecl GhcPs -> WriterT Any Hsc (LHsDecl GhcPs) 
transformDecl = Uniplate.transformBiM \case
  lit@(L l (HsLit _ (HsString _ _))) -> do
    tell $ Any True
    pure $ exprWithTySig lit
  other -> pure other
  where
    exprWithTySig :: LHsExpr GhcPs -> LHsExpr GhcPs
    exprWithTySig lit@(L l _) = L l $ ExprWithTySig defExt lit $ mkHsWildCardBndrs $ HsIB defExt $ textT l
    textT :: SrcSpan -> LHsType GhcPs
    textT l = L l $ HsTyVar defExt NotPromoted $ L l $ mkRdrQual dataTextModule $ mkTcOcc textTyName 
    textTyName = "Text"

dataTextModule = mkModuleName "Data.Text"

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
transformDecl = Uniplate.descendBiM goBi
  where
    goBi :: LHsExpr GhcPs -> WriterT Any Hsc (LHsExpr GhcPs)
    goBi = \case
      lit@(L l (HsLit _ (HsString _ _))) -> do
        tell $ Any True
        pure $ L l $ HsPar defExt $ L l $ ExprWithTySig defExt lit $ mkHsWildCardBndrs $ HsIB defExt $ textT l 

        -- bypass already explicitly annotated types  
      pass@(L _ (ExprWithTySig _ _ _)) -> pure pass
      other -> Uniplate.descendM goBi other

    textT :: SrcSpan -> LHsType GhcPs
    textT l = L l $ HsTyVar defExt NotPromoted $ L l $ mkRdrQual dataTextModule $ mkTcOcc textTyName 
    textTyName = "Text"

dataTextModule = mkModuleName "Data.Text"

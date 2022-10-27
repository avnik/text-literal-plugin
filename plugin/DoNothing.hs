{-# LANGUAGE CPP #-}
module DoNothing (plugin) where
import qualified GHC
#if __GLASGOW_HASKELL__ > 901
import qualified GHC.Types.SourceText as GHC
#elif __GLASGOW_HASKELL__ >= 900
import qualified GHC.Driver.Types as GHC
#endif
#if __GLASGOW_HASKELL__ < 900
import qualified GhcPlugins as GHC
#else
import qualified GHC.Driver.Plugins as GHC
import qualified GHC.Plugins as GHC
#endif

-- | GHC plugin.
plugin :: GHC.Plugin
plugin = GHC.defaultPlugin
  { GHC.pluginRecompile = GHC.purePlugin -- Adding THIS line triggering segfault
  }

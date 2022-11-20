{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators    #-}

{-# OPTIONS_GHC -fplugin=Data.Record.Anon.Plugin -fplugin=RecordDotPreprocessor -fplugin=Data.Text.Literal.Plugin #-}
{-# OPTIONS_GHC -ddump-rn-ast #-}
module Main where
import Data.Maybe
import Data.Text as T
import Data.Text.IO as TIO

import Data.Record.Anon
import Data.Record.Anon.Simple (Record)
import qualified Data.Record.Anon.Simple as Anon


{-------------------------------------------------------------------------------
  Example values
-------------------------------------------------------------------------------}

recordA :: Record [ "a" := Bool, "b" := T.Text, "mb" := Maybe T.Text ]
recordA = ANON { 
    a = True,
    b = "Text",
    -- mb = Just ("Maybe Text" :: Text)
    mb = Just "Maybe Text"
}


main :: IO ()
main = TIO.putStrLn recordA.b

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fplugin=Data.Text.Literal.Plugin #-}
module Tests.Sanity (sanity) where

import Test.Tasty
import Test.Tasty.HUnit

-- intentionally as T, we check that plugin correctly insert `import qualified Data.Text`
import qualified Data.Text as T

class AsText a where
  asText :: a -> Bool

instance AsText String where
  asText _ = False

instance AsText T.Text where
  asText _ = True 

sanity :: Assertion
sanity = do
  asText "text" @?= True

{-# LANGUAGE TypeSynonymInstances #-}
module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Tests.Sanity

main :: IO ()
main = defaultMain tests

--
-- Test intentionally split into two modules, TestTree expects 'String', not 'Text'

tests :: TestTree
tests = testGroup "Tests" [
    testCase "sanity" sanity
  ]

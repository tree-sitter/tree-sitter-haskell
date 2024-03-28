module Main where

import TSH ()
import TSH.Test.GapsTest (test_gaps)
import TSH.Test.Utils (unitTest)
import Test.Tasty (TestTree, defaultMain, testGroup)

tests :: TestTree
tests =
  testGroup "all" [
    unitTest "split gaps" test_gaps
  ]

main :: IO ()
main =
  defaultMain tests

module Main (main) where

import           Test.Tasty
import           Test.Tasty.Ingredients.Rerun

import           UnitTests
import           Properties

main :: IO ()
main = do
  tests1 <- unitTestFolder "examples/"
  tests2 <- compileTestFolder "examples/"
  tests3 <- compileVideoFolder "videos/"
  defaultMainWithRerun $ testGroup "tests" [tests1, tests2, tests3, all_props]

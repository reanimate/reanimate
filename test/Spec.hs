module Main (main) where

import           Test.Tasty
import           Test.Tasty.Ingredients.Rerun (rerunningTests)
import           Test.Tasty.Runners

import           Properties
import           UnitTests

main :: IO ()
main = do
  tests1 <- unitTestFolder "examples/"
  tests2 <- compileTestFolder "examples/"
  tests3 <- compileVideoFolder "videos/"
  defaultMainWithRerun $ testGroup "tests" [tests1, tests2, tests3, all_props]

-- Copy of 'defaultMainWithRerun' because that function is not available in lts-12.
defaultMainWithRerun :: TestTree -> IO ()
defaultMainWithRerun = defaultMainWithIngredients
    [ rerunningTests [ listingTests, consoleTestReporter ] ]

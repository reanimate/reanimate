module Main (main) where

import           Test.Tasty

import           UnitTests

main :: IO ()
main = do
  tests1 <- unitTestFolder "examples/"
  tests2 <- compileTestFolder "examples/"
  defaultMain $ testGroup "tests" [tests1, tests2]

module Main (main) where

import           Test.Tasty

import           UnitTests

main :: IO ()
main = do
  tests1 <- unitTestFolder "examples/"
  tests2 <- compileTestFolder "examples/"
  tests3 <- compileVideoFolder "videos/"
  defaultMain $ testGroup "tests" [tests1, tests2, tests3]

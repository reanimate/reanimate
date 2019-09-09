module Main (main) where

import           Test.Tasty

import           UnitTests

main :: IO ()
main = do
  ts <- unitTestFolder "examples/"
  defaultMain ts

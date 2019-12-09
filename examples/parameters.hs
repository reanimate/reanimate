#!/usr/bin/env stack
-- stack runghc --package reanimate
module Main (main) where

import           Reanimate
import qualified Data.Text as T

main :: IO ()
main = reanimate $ animate $ const $
    mkText $ T.pack $ "Test parameters: " ++ show (pFPS, pWidth, pHeight)


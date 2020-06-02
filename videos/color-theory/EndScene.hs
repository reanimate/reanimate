{-# LANGUAGE OverloadedStrings #-}
module EndScene (endScene) where

import           Reanimate
import           Reanimate.Builtin.Images
import           Transcript

endScene :: Scene s ()
endScene = do
  newSpriteSVG_ $ scale 0.5 $ githubWhiteIcon
  waitUntil $ wordEnd (findWord ["end"] "domain") + 1

module Reanimate.Builtin.Slide where

import Reanimate.Builtin.Flip
import Reanimate.Scene
import Reanimate.Animation
import Reanimate.Constants
import Reanimate.Svg

-- | <<docs/gifs/doc_slide.gif>>
slide :: Transition
slide a b = sceneAnimation $ do
  s1 <- fork $ newSpriteA a
  s2 <- fork $ newSpriteA b
  fork $ spriteTween s1 dur $ \t -> translate (-screenWidth*t) 0
  fork $ spriteTween s2 dur $ \t -> translate (screenWidth-screenWidth*t) 0
  where
    dur = max (duration a) (duration b)

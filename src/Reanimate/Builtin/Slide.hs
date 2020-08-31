{-|
Copyright   : Written by David Himmelstrup
License     : Unlicense
Maintainer  : lemmih@gmail.com
Stability   : experimental
Portability : POSIX
-}
module Reanimate.Builtin.Slide where

import Reanimate.Transition
import Reanimate.Constants
import Reanimate.Svg
import Reanimate.Effect

-- | <<docs/gifs/doc_slideLeftT.gif>>
slideLeftT :: Transition
slideLeftT = effectT slideLeft (andE slideLeft moveRight)
  where
    slideLeft = translateE (-screenWidth) 0
    moveRight = constE (translate screenWidth 0)
    andE a b d t = a d t . b d t

-- | <<docs/gifs/doc_slideDownT.gif>>
slideDownT :: Transition
slideDownT = effectT slideDown (andE slideDown moveUp)
  where
    slideDown = translateE 0 (-screenHeight)
    moveUp = constE (translate 0 screenHeight)
    andE a b d t = a d t . b d t

-- | <<docs/gifs/doc_slideUpT.gif>>
slideUpT :: Transition
slideUpT = effectT slideUp (andE slideUp moveDown)
  where
    slideUp = translateE 0 screenHeight
    moveDown = constE (translate 0 (-screenHeight))
    andE a b d t = a d t . b d t

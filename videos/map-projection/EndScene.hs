module EndScene (endScene) where

import           Reanimate
import           Reanimate.Builtin.Images

endScene :: Animation
endScene = mkAnimation 10 $ const $
  mkGroup
  [ mkBackground "black"
  , scale 0.5 $ githubIcon ]

background = "lightblue"

shape :: SVG
shape = mkCircle 4
--shape = mkRect 6 6
--shape = mkLine (screenLeft, screenBottom) (screenRight, screenTop)

animation :: Animation
animation = docEnv $
  addStatic (mkBackground background) $
  playThenReverseA $
  signalA (curveS 2) $
  setDuration 3 $ animate $ \t ->
  partialSvg t $ pathify shape

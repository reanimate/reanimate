animation :: Animation
animation = docEnv $ sceneAnimation $ do
  play $ drawBox `parA` drawCircle
    & label "parA"
  play $ drawBox `seqA` drawCircle
    & label "seqA"
  play $ drawBox `andThen` drawCircle
    & label "andThen"

label txt = addStatic $
  withFillOpacity 1 $ withStrokeWidth 0 $
  withFillColor "black" $
  translate screenLeft (screenBottom+0.2) $
  latex txt

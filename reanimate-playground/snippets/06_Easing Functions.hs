animation :: Animation
animation = docEnv $ pauseAtEnd 1 $ sceneAnimation $ do
  showEasing 0 "curveS" (curveS 2)
  showEasing 1 "bellS" (bellS 2)
  showEasing 2 "constantS" (constantS 0.7)
  showEasing 3 "oscillateS" oscillateS
  showEasing 4 "powerS" (powerS 2)
  showEasing 5 "reverseS" reverseS
  showEasing 6 "id" id

showEasing nth txt fn = do
  let yOffset | even nth = 0
              | odd nth  = -1
      xOffset = -6 + fromIntegral nth*2
  newSpriteSVG_ $
  	translate xOffset yOffset $
    label txt
  fork $ play $ mapA (translate xOffset 0) $
    mapA (rotate 90) $
    mapA (scale (screenHeight/screenWidth * 0.6)) $
    signalA fn drawProgress

label txt =
  translate 0 (-2.5) $
  scale 0.7 $
  center $
  withStrokeWidth 0 $
  withFillOpacity 1 $
  svg
  where
    svg = latex txt

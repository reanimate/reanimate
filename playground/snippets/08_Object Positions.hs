env =
  addStatic (mkBackground "white") .
  mapA (withStrokeColor "black")

animation :: Animation
animation = env $
  sceneAnimation $ do
    -- Configure objects
    txt <- newText "Center"
    top <- newText "Top"
    oModifyS top $ 
      oTopY .= screenTop
    topR <- newText "Top right"
    oModifyS topR $ do
      oTopY .= screenTop
      oRightX .= screenRight
    botR <- newText "Bottom right"
    oModifyS botR $ do
      oTranslate .= (0, screenBottom+0.5)
      oRightX .= screenRight
    botL <- newText "Bottom left"
    oModifyS botL $ do
      oTranslate .= (0, screenBottom+0.5)
      oLeftX .= screenLeft
    topL <- newText "Top left"
    oModifyS topL $ do
      oTopY .= screenTop
      oLeftX .= screenLeft
    -- Show objects
    oShow txt
    wait 1
    switchTo txt top
    switchTo top topR
    switchTo topR botR
    switchTo botR botL
    switchTo botL topL
    switchTo topL txt

switchTo src dst = do
  fork $ oFadeOut src 1
  oModify dst $ oOpacity .~ 1
  oFadeIn dst 1
  wait 1

newText txt =
  newObject $ scale 1.5 $ centerX $ latex txt

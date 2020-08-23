env =
  addStatic (mkBackground "white") .
  mapA (withStrokeColor "black")

animation :: Animation
animation = env $
  sceneAnimation $ do
    circ <- newObject $ Circle 3
    oModify circ $
      oContext .~ withFillColor "pink"
    box <- newObject $ Rectangle 5 5
    oModify box $
      oContext .~ withFillColor "lightblue"
  
    oGrow circ 1; wait 1
    oTransform circ box 1; wait 1
    oFadeOut box 1; wait 1

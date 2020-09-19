env =
  addStatic (mkBackground "white") .
  mapA (withStrokeColor "black")

animation :: Animation
animation = env $
  scene $ do
    circ <- newObject $ Circle 3
    oModify circ $
      oContext .~ withFillColor "pink"
    box <- newObject $ Rectangle 5 5
    oModify box $
      oContext .~ withFillColor "lightblue"
  
    oShowWith circ oGrow; wait 1
    oTransform circ box 1; wait 1
    oHideWith box oFadeOut; wait 1

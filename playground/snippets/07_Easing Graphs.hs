colorPalette = parula -- try: viridis, sinebow, turbo, cividis
fns =
 [("curveS", curveS 2)
 ,("bellS", bellS 2)
 ,("constantS", constantS 0.7)
 ,("oscillateS", oscillateS)
 ,("powerS", powerS 2)
 ,("reverseS", reverseS)
 ,("id", id)
 ]

animation :: Animation
animation = docEnv $ pauseAtEnd 1 $ sceneAnimation $ do
  newSpriteSVG_ $ mkBackground "white"
  play $ signalA (curveS 2) $ animate $ \t -> partialSvg t grid
  newSpriteSVG_ grid
  wait 1
  flip mapM_ (zip [0..] fns) $ \(nth, (txt, fn)) -> do
    let color = promotePixel $ colorPalette (nth / fromIntegral (length fns-1))
    showEasing nth txt fn color
  {- showEasing 3 "oscillateS" oscillateS
  showEasing 4 "powerS" (powerS 2)
  showEasing 5 "reverseS" reverseS
  showEasing 6 "id" id -}

gridOffset = -2
gridHeight = 5
gridWidth = 8

grid :: SVG
grid = translate gridOffset 0 $
  withStrokeWidth defaultStrokeWidth $
  withStrokeColor "grey" $ mkGroup
  [ mkPath $ concat
    [[ SVG.MoveTo SVG.OriginAbsolute [V2 (-gridWidth/2) (gridHeight/2-n)]
     ,SVG.HorizontalTo SVG.OriginRelative [gridWidth] ]
    | n <- [1..gridHeight-1]
    ]
  , mkPath 
    [ SVG.MoveTo SVG.OriginAbsolute [V2 (-gridWidth/2) (gridHeight/2)]
    , SVG.VerticalTo SVG.OriginRelative [-gridHeight]
    , SVG.HorizontalTo SVG.OriginRelative [gridWidth]
    , SVG.VerticalTo SVG.OriginRelative [gridHeight]
    , SVG.EndPath
    ]
  ]

showEasing nth txt fn color = do
  let steps = 100
      slope = withStrokeColorPixel color $
        translate gridOffset 0 $ mkLinePath
        [ ((x/steps-0.5)*gridWidth, (y-0.5)*gridHeight)
        | x <- [0..steps]
        , let y = fn (x/steps) ]
  s <- newSpriteSVG $
    withFillColorPixel color $
    translate (gridWidth/2+gridOffset+0.5) (gridHeight/2-nth) $
      label txt
  spriteE s $ overBeginning 0.2 fadeInE
  play $ animate $ \t -> partialSvg t slope
  newSpriteSVG_ slope
  wait 1
  
label txt = withStrokeColor "black" $
  withStrokeWidth (defaultStrokeWidth*2) $
  withFillOpacity 1 $
  latex txt

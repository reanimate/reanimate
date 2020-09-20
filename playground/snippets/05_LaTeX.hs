env =
  addStatic (mkBackground "white") .
  mapA (withStrokeColor "black")

animation :: Animation
animation = env $
  scene $ do
    drawLatex "e^{i\\pi}+1=0"
    drawLatex "\\sum_{k=1}^\\infty {1 \\over k^2} = {\\pi^2 \\over 6}"
    drawLatex "\\sum_{k=1}^\\infty"
    wait 1

drawLatex txt = do
    -- Draw outline
    fork $ do
      play $ animate $ \t ->
        withFillOpacity 0 $
        partialSvg t svg
      -- Fade outline
      play $ animate $ \t ->
        withFillOpacity 0 $
        withStrokeWidth (defaultStrokeWidth*(1-t))
        svg
    wait 0.7
    -- Fill in letters
    play $ animate $ \t ->
      withFillOpacity t $ withStrokeWidth 0
      svg
    -- Hold static image and then fade out
    play $ staticFrame 2 (withStrokeWidth 0 svg)
      & applyE (overEnding 0.3 fadeOutE)
  where
    svg = scale 2 $ center $ latexAlign txt

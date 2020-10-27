fonts :: [(T.Text, TexConfig)]
fonts =
  [ ("Computer Modern", defaultCfg)
  , ("Calligra", calligra)
  , ("Noto", noto)
  , ("Helvetica", helvet)
  , ("Liberine", libertine) ]

animation :: Animation
animation = env $
  scene $ do
    forM_ fonts $ \(name, cfg) -> do
      play $ showCfg name cfg
        & setDuration 2
        & applyE (overBeginning 0.3 fadeInE)
        & applyE (overEnding 0.3 fadeOutE)

defaultCfg = TexConfig LaTeX [] []

showCfg :: T.Text -> TexConfig -> Animation
showCfg name cfg = scene $ do
  let title = scale 2 $ center $
        latexCfg cfg name
      line1 = center $ latexCfg cfg
        "Pack My Box"
      line2 = center $ latexCfg cfg
        "With Five Dozen"
      line3 = center $ latexCfg cfg
        "Liquour Jugs"

  header <- oNew title
  oModify header $
    oTopY .~ screenTop
  oShow header

  l1 <- oNew line1
  l1 `oBelow` header
  oShow l1

  l2 <- oNew line2
  l2 `oBelow` l1
  oShow l2

  l3 <- oNew line3
  l3 `oBelow` l2
  oShow l3
  wait 1

oBelow a b = do
  aBot <- oRead b oBottomY
  oModifyS a $ do
    oMarginTop .= 0
    oTopY .= aBot

env =
  addStatic (mkBackground "white") .
  mapA (withStrokeWidth 0)


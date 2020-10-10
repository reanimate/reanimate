showCfg :: T.Text -> TexConfig -> Scene s ()
showCfg name cfg = do
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

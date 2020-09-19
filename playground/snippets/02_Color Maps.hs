animation :: Animation
animation = docEnv $ scene $ do
  play $ staticFrame 1 (showColorMap parula)
    & label "Parula"
  play $ staticFrame 1 (showColorMap viridis)
    & label "Viridis"
  play $ staticFrame 1 (showColorMap turbo)
    & label "Turbo"
  play $ staticFrame 1 (showColorMap greyscale)
    & label "Greyscale"

label txt = overlay $
  withFillOpacity 1 $ withStrokeWidth 0 $
  withFillColor "white" $
  translate screenLeft (screenBottom+0.2) $
  latex txt

overlay svg ani = ani `parA` staticFrame (duration ani) svg

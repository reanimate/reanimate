module Reanimate.Driver.CLI
  ( getDriverOptions
  , Options(..)
  , Command(..)
  , Preset(..)
  , Format(..)
  , Raster(..)
  , showFormat
  , showRaster
  ) where

import           Data.Char
import           Data.Monoid
import           Options.Applicative
import           Prelude
import           Reanimate.Render    (FPS, Format (..), Height, Raster (..),
                                      Width)

newtype Options = Options
  { optsCommand :: Command
  } deriving (Show)

data Command
  = Raw
    { rawOutputFolder :: FilePath
    , rawFrameOffset  :: Int
    , rawPrettyPrint  :: Bool
    }
  | Test
  | Check
  | View
    { viewVerbose   :: Bool
    , viewGHCPath   :: Maybe FilePath
    , viewGHCOpts   :: [String]
    , viewOrigin    :: Maybe FilePath
    }
  | Render
    { renderTarget  :: Maybe String
    , renderFPS     :: Maybe FPS
    , renderWidth   :: Maybe Width
    , renderHeight  :: Maybe Height
    , renderCompile :: Bool
    , renderFormat  :: Maybe Format
    , renderPreset  :: Maybe Preset
    , renderRaster  :: Raster
    , renderPartial :: Bool
    , renderHash    :: Bool
    }
   deriving (Show)

data Preset = Youtube | ExampleGif | Quick | MediumQ | HighQ | LowFPS
  deriving (Show)

readRaster :: String -> Maybe Raster
readRaster raster =
  case map toLower raster of
    "none"          -> Just RasterNone
    "auto"          -> Just RasterAuto
    "inkscape"      -> Just RasterInkscape
    "rsvg"          -> Just RasterRSvg
    "imagemagick"   -> Just RasterMagick
    _               -> Nothing

showRaster :: Raster -> String
showRaster RasterNone     = "none"
showRaster RasterAuto     = "auto"
showRaster RasterInkscape = "inkscape"
showRaster RasterRSvg     = "rsvg"
showRaster RasterMagick   = "imagemagick"

readFormat :: String -> Maybe Format
readFormat fmt =
  case map toLower fmt of
    "mp4"  -> Just RenderMp4
    "gif"  -> Just RenderGif
    "webm" -> Just RenderWebm
    _      -> Nothing

showFormat :: Format -> String
showFormat RenderMp4  = "mp4"
showFormat RenderGif  = "gif"
showFormat RenderWebm = "webm"

readPreset :: String -> Maybe Preset
readPreset preset =
  case map toLower preset of
    "youtube" -> Just Youtube
    "gif"     -> Just ExampleGif
    "quick"   -> Just Quick
    "medium"  -> Just MediumQ
    "high"    -> Just HighQ
    "lowfps"  -> Just LowFPS
    _         -> Nothing

showPreset :: Preset -> String
showPreset Youtube    = "youtube"
showPreset ExampleGif = "gif"
showPreset Quick      = "quick"
showPreset MediumQ    = "medium"
showPreset HighQ      = "high"
showPreset LowFPS     = "lowfps"

options :: Parser Options
options = Options <$> commandP

commandP :: Parser Command
commandP = subparser(
     command "test" testCommand
  <> commandGroup "Internal commands"
  <> internal )
  <|> hsubparser
    ( command "check" checkCommand
    <> command "view" viewCommand
    <> command "render" renderCommand
    <> command "raw" rawCommand
    )
  <|> infoParser viewCommand

rawCommand :: ParserInfo Command
rawCommand = info parse
    (progDesc "Output raw SVGs for animation at 60 fps. Used internally by viewer.")
  where
    parse = Raw
      <$> strOption
        ( long "output" <>
          short 'o' <>
          metavar "PATH" <>
          help "Output folder" <>
          value ".")
      <*> option auto
        ( long "offset" <>
          metavar "NUMBER" <>
          help "Frame offset" <>
          value 0)
      <*> switch
        ( long "pretty-print" <>
          short 'p' <>
          help "Pretty print svg")

testCommand :: ParserInfo Command
testCommand = info (parse <**> helper)
    (progDesc "Generate 10 frames spread out evenly across the animation. Used \
              \internally by the test-suite.")
  where
    parse = pure Test

checkCommand :: ParserInfo Command
checkCommand = info parse
    (progDesc "Run a system's diagnostic and report any missing external dependencies.")
  where
    parse = pure Check

viewCommand :: ParserInfo Command
viewCommand = info parse
    (progDesc "Play animation in browser window.")
  where
    parse = View
      <$> switch
        (long "verbose" <> short 'v')
      <*> optional (strOption (long "ghc"
                    <> metavar "PATH"
                    <> help "Path to GHC binary"))
      <*> many (strOption (long "ghc-opt"
                <> short 'G'
                <> help "Additional option to pass to ghc"))
      <*> optional (strOption (long "self"
                    <> metavar "PATH"
                    <> help "Source file used for live-reloading"))

renderCommand :: ParserInfo Command
renderCommand = info parse
    (progDesc "Render animation to file.")
  where
    -- fromPreset :: (Maybe Preset -> (Command -> Command))
    -- fromPreset Nothing = id
    -- fromPreset (Just ExampleGif) = \cmd -> cmd{renderFPS=24}
    -- modParser :: Parser (Command -> Command)
    -- modParser = fmap fromPreset $
    --   optional (option (maybeReader readPreset)
    --       (long "preset" <> showDefaultWith showPreset
    --       <> metavar "TYPE"
    --       <> help "Parameter presets: youtube, gif, quick"))
    parse = Render
      <$> optional (strOption (long "target"
                    <> short 'o'
                    <> metavar "FILE"
                    <> help "Write output to FILE"))
      <*> optional (option auto
          (long "fps" <> metavar "FPS"
          <> help "Set frames per second."))
      <*> optional (option auto
          (long "width" <> short 'w' <> metavar "PIXELS"
          <> help "Set video width."))
      <*> optional (option auto
          (long "height" <> short 'h'
          <> metavar "PIXELS" <> help "Set video height."))
      <*> switch (long "compile"
                  <> help "Compile source code before rendering.")
      <*> optional (option (maybeReader readFormat)
          (long "format" <> metavar "FMT"
          <> help "Video format: mp4, gif, webm"))
      <*> optional (option (maybeReader readPreset)
          (long "preset" <> showDefaultWith showPreset
          <> metavar "TYPE"
          <> help "Parameter presets: youtube, gif, quick, medium, high"))
      <*> option (maybeReader readRaster)
          (long "raster" <> showDefaultWith showRaster
          <> metavar "RASTER"
          <> value RasterNone
          <> help "Raster engine: none, auto, inkscape, rsvg, imagemagick")
      <*> switch
        (long "partial"
        <> help "Produce partial animation even if frame generation was \
                \interrupted by ctrl-c")
      <*> flag True False
        (long "disable-hashing"
        <> help "Disable SVG dedup via hashing. This might improve performance \
                \if all your frames are unique.")

opts :: ParserInfo Options
opts = info (options <**> helper )
  ( fullDesc
  <> progDesc "This program contains an animation which can either be viewed \
              \in a web-browser or rendered to disk."
  )

getDriverOptions :: IO Options
getDriverOptions = customExecParser (prefs showHelpOnError) opts

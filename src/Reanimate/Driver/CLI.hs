{-# LANGUAGE RecordWildCards #-}
module Reanimate.Driver.CLI
  ( getDriverOptions
  , Options(..)
  , Command(..)
  , Preset(..)
  , Format(..)
  , showFormat
  ) where

import           Data.Char
import           Data.Monoid         ((<>))
import           Options.Applicative
import           Reanimate.Render    (Format (..), Width, Height, FPS)

data Options = Options
  { optsCommand :: Command
  } deriving (Show)

data Command
  = Raw
  | Test
  | Check
  | View
  | Render
    { renderTarget  :: Maybe String
    , renderFPS     :: Maybe FPS
    , renderWidth   :: Maybe Width
    , renderHeight  :: Maybe Height
    , renderCompile :: Bool
    , renderFormat  :: Maybe Format
    , renderPreset  :: Maybe Preset
    }
   deriving (Show)

data Preset = Youtube | ExampleGif | Quick
  deriving (Show)

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
    _         -> Nothing

showPreset :: Preset -> String
showPreset Youtube    = "youtube"
showPreset ExampleGif = "gif"
showPreset Quick      = "quick"

options :: Parser Options
options = Options <$> commandP

commandP :: Parser Command
commandP = subparser(command "raw" rawCommand
  <> command "test" testCommand
  <> commandGroup "Internal commands"
  <> internal )
  <|> hsubparser
    ( command "check" checkCommand
    <> command "view" viewCommand
    <> command "render" renderCommand
    )
  <|> infoParser viewCommand

rawCommand :: ParserInfo Command
rawCommand = info (parse)
    (progDesc "Output raw SVGs for animation at 60 fps. Used internally by viewer.")
  where
    parse = pure Raw

testCommand :: ParserInfo Command
testCommand = info (parse <**> helper)
    (progDesc "Generate 50 frames spread out evenly across the animation. Used \
              \internally by the test-suite.")
  where
    parse = pure Test

checkCommand :: ParserInfo Command
checkCommand = info (parse)
    (progDesc "check")
  where
    parse = pure Check

viewCommand :: ParserInfo Command
viewCommand = info (parse)
    (progDesc "view")
  where
    parse = pure View

renderCommand :: ParserInfo Command
renderCommand = info (parse)
    (progDesc "render")
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
          <> help "Parameter presets: youtube, gif, quick"))

opts :: ParserInfo Options
opts = info (options <**> helper )
  ( fullDesc
  <> progDesc "PROG DESC"
  <> header "HEADER"
  )

getDriverOptions :: IO Options
getDriverOptions = customExecParser (prefs showHelpOnError) opts

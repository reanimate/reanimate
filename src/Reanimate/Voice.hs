{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
{-|
  Reanimate can automatically synchronize animations to your voice if you have
  a transcript and an audio recording. This works with the help of Gentle
  (<https://lowerquality.com/gentle/>). Accuracy is not perfect but it is pretty
  close, and it is by far the easiest way of adding narration to an animation.
-}
module Reanimate.Voice
  ( Transcript(..)
  , TWord(..)
  , Phone(..)
  , findWord                -- :: Transcript -> [Text] -> Text -> TWord
  , findWords               -- :: Transcript -> [Text] -> Text -> [TWord]
  , loadTranscript          -- :: FilePath -> Transcript
  , fakeTranscript          -- :: Text -> Transcript
  , splitTranscript         -- :: Transcript -> SVG -> [(SVG, TWord)]
  , annotateWithTranscript  -- :: Transcript -> Scene s ()
  )
where

import           Data.Aeson
import           Data.Char
import           System.IO.Unsafe                         ( unsafePerformIO )
import           System.Directory
import           System.FilePath
import           System.Process
import           System.Exit
import           Control.Monad
import           Data.List
import           Data.Maybe
import qualified Data.Map                      as Map
import           Data.Map                                 ( Map )
import           Data.Text                                ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Reanimate.Misc
import           Reanimate.LaTeX
import           Reanimate.Scene
import           Reanimate.Animation
import           Reanimate.Svg
import           Reanimate.Constants

-- | Aligned transcript. Contains the transcript text as well as
--   timing data for each word.
data Transcript = Transcript
  { transcriptText  :: Text
  , transcriptKeys  :: Map Text Int
  , transcriptWords :: [TWord]
  } deriving (Show)

instance FromJSON Transcript where
  parseJSON = withObject "transcript" $ \o ->
    Transcript <$> o .: "transcript" <*> pure Map.empty <*> o .: "words"

-- | Spoken word. Includes information about when it was spoken,
--   its duration, and its phonemes.
data TWord = TWord
  { wordAligned     :: Text
  , wordCase        :: Text
  , wordStart       :: Double -- ^ Start of pronunciation in seconds
  , wordStartOffset :: Int    -- ^ Character index of word in transcript
  , wordEnd         :: Double -- ^ End of pronunciation in seconds
  , wordEndOffset   :: Int    -- ^ Last character index of word in transcript
  , wordPhones      :: [Phone]
  , wordReference   :: Text   -- ^ The word being pronounced.
  } deriving (Show)

instance FromJSON TWord where
  parseJSON = withObject "word" $ \o ->
    TWord
      <$> o
      .:? "alignedWord"
      .!= T.empty
      <*> o
      .:  "case"
      <*> o
      .:? "start"
      .!= 0
      <*> o
      .:  "startOffset"
      <*> o
      .:? "end"
      .!= 0
      <*> o
      .:  "endOffset"
      <*> o
      .:? "phones"
      .!= []
      <*> o
      .:  "word"

-- | Phoneme type
data Phone = Phone
  { phoneDuration :: Double
  , phoneType     :: Text
  } deriving (Show)

instance FromJSON Phone where
  parseJSON =
    withObject "phone" $ \o -> Phone <$> o .: "duration" <*> o .: "phone"

-- | Locate the first word that occurs after all the given keys.
--   An error is thrown if no such word exists. An error is thrown
--   if the keys do not exist in the transcript.
findWord :: Transcript -> [Text] -> Text -> TWord
findWord t keys w = case listToMaybe (findWords t keys w) of
  Nothing    -> error $ "Word not in transcript: " ++ show (keys, w)
  Just tword -> tword

-- | Locate all words that occur after all the given keys.
--   May return an empty list. An error is thrown
--   if the keys do not exist in the transcript.
findWords :: Transcript -> [Text] -> Text -> [TWord]
findWords t [] wd =
  [ tword | tword <- transcriptWords t, wordReference tword == wd ]
findWords t (key : keys) wd =
  [ tword
  | tword <- findWords t keys wd
  , wordStartOffset tword >= Map.findWithDefault badKey key (transcriptKeys t)
  ]
  where badKey = error $ "Missing transcript key: " ++ show key

-- | Loading a transcript does three things depending on which files are available
--   with the same basename as the input argument:
--   1. If a JSON file is available, it is parsed and returned.
--   2. If an audio file is available, reanimate tries to align it by calling out to
--      Gentle on localhost:8765/. If Gentle is not running, an error will be thrown.
--   3. If only the text transcript is available, a fake transcript is returned,
--      with timings roughly at 120 words per minute.
loadTranscript :: FilePath -> Transcript
loadTranscript path = unsafePerformIO $ do
  rawTranscript <- T.readFile path
  let keys           = parseTranscriptKeys rawTranscript
      trimTranscript = cutoutKeys keys rawTranscript
  hasJSON    <- doesFileExist jsonPath
  transcript <- if hasJSON
    then do
      mbT <- decodeFileStrict jsonPath
      case mbT of
        Nothing -> error "bad json"
        Just t  -> pure t
    else do
      hasAudio <- findWithExtension path audioExtensions
      case hasAudio of
        Nothing        -> return $ fakeTranscript' trimTranscript
        Just audioPath -> withTempFile "txt" $ \txtPath -> do
          T.writeFile txtPath trimTranscript
          runGentleForcedAligner audioPath txtPath
          mbT <- decodeFileStrict jsonPath
          case mbT of
            Nothing -> error "bad json"
            Just t  -> pure t
  pure $ transcript { transcriptKeys = finalizeKeys keys }
 where
  jsonPath        = replaceExtension path "json"
  audioExtensions = ["mp3", "m4a", "flac"]

parseTranscriptKeys :: Text -> Map Text Int
parseTranscriptKeys = worker Map.empty 0
 where
  worker keys offset txt = case T.uncons txt of
    Nothing -> keys
    Just ('[', cs) ->
      let key       = T.takeWhile (/= ']') cs
          newOffset = T.length key + 2
      in  worker (Map.insert key offset keys)
                 (offset + newOffset)
                 (T.drop newOffset txt)
    Just (_, cs) -> worker keys (offset + 1) cs

finalizeKeys :: Map Text Int -> Map Text Int
finalizeKeys = Map.fromList . worker 0 . sortOn snd . Map.toList
 where
  worker _offset [] = []
  worker offset ((key, at) : rest) =
    (key, at - offset) : worker (offset + T.length key + 2) rest

cutoutKeys :: Map Text Int -> Text -> Text
cutoutKeys keys = T.concat . worker 0 (sortOn snd (Map.toList keys))
 where
  worker _offset [] txt = [txt]
  worker offset ((key, at) : xs) txt =
    let keyLen          = T.length key + 2
        (before, after) = T.splitAt (at - offset) txt
    in  before : worker (at + keyLen) xs (T.drop keyLen after)

findWithExtension :: FilePath -> [String] -> IO (Maybe FilePath)
findWithExtension _path []       = return Nothing
findWithExtension path  (e : es) = do
  let newPath = replaceExtension path e
  hasFile <- doesFileExist newPath
  if hasFile then return (Just newPath) else findWithExtension path es

runGentleForcedAligner :: FilePath -> FilePath -> IO ()
runGentleForcedAligner audioFile transcriptFile = do
  ret <- rawSystem prog args
  case ret of
    ExitSuccess -> return ()
    ExitFailure e ->
      error
        $  "Gentle forced aligner failed with: "
        ++ show e
        ++ "\nIs it running locally on port 8765?"
        ++ "\nCommand: "
        ++ showCommandForUser prog args
 where
  prog = "curl"
  args =
    [ "--silent"
    , "--form"
    , "audio=@" ++ audioFile
    , "--form"
    , "transcript=@" ++ transcriptFile
    , "--output"
    , replaceExtension audioFile "json"
    , "http://localhost:8765/transcriptions?async=false"
    ]

data Token = TokenWord Int Int Text | TokenComma | TokenPeriod | TokenParagraph
  deriving (Show)

lexText :: Text -> [Token]
lexText = worker 0
 where
  worker offset txt = case T.uncons txt of
    Nothing -> []
    Just (c, cs)
      | isSpace c
      -> let (w, rest) = T.span (== '\n') txt
         in  if T.length w >= 3
               then TokenParagraph : worker (offset + T.length w) rest
               else worker (offset + 1) cs
      | c == '.'
      -> TokenPeriod : worker (offset + 1) cs
      | c == ','
      -> TokenComma : worker (offset + 1) cs
      | isWord c
      -> let (w, rest) = T.span isWord txt
             newOffset = offset + T.length w
         in  TokenWord offset newOffset w : worker newOffset rest
      | otherwise
      -> worker (offset + 1) cs
  isWord c = isAlphaNum c || c `elem` ['\'', '-']

-- | Fake transcript timings at roughly 120 words per minute.
fakeTranscript :: Text -> Transcript
fakeTranscript rawTranscript =
  let keys = parseTranscriptKeys rawTranscript
      t    = fakeTranscript' (cutoutKeys keys rawTranscript)
  in  t { transcriptKeys = finalizeKeys keys }

fakeTranscript' :: Text -> Transcript
fakeTranscript' input = Transcript { transcriptText  = input
                                   , transcriptKeys  = Map.empty
                                   , transcriptWords = worker 0 (lexText input)
                                   }
 where
  worker _now []             = []
  worker now  (token : rest) = case token of
    TokenWord start end w ->
      let dur = realToFrac (end - start) * 0.1
      in  TWord { wordAligned     = T.toLower w
                , wordCase        = "success"
                , wordStart       = now
                , wordStartOffset = start
                , wordEnd         = now + dur
                , wordEndOffset   = end
                , wordPhones      = []
                , wordReference   = w
                }
            : worker (now + dur) rest
    TokenComma     -> worker (now + commaPause) rest
    TokenPeriod    -> worker (now + periodPause) rest
    TokenParagraph -> worker (now + paragraphPause) rest
  paragraphPause = 0.5
  commaPause     = 0.1
  periodPause    = 0.2

-- | Convert the transcript text to an SVG image using LaTeX and associate
--   each word image with its timing information.
splitTranscript :: Transcript -> [(SVG, TWord)]
splitTranscript Transcript {..} =
  [ (svg, tword)
  | tword@TWord {..} <- transcriptWords
  , let wordLength  = wordEndOffset - wordStartOffset
        [_, svg, _] = latexChunks
          [ T.take wordStartOffset transcriptText
          , T.take wordLength (T.drop wordStartOffset transcriptText)
          , T.drop wordEndOffset transcriptText
          ]
  ]

-- | Helper function for rendering a transcript.
annotateWithTranscript :: Transcript -> Scene s ()
annotateWithTranscript t = forM_ (transcriptWords t) $ \tword -> do
  let svg = scale 1 $ latex (wordReference tword)
  waitUntil (wordStart tword)
  let dur = wordEnd tword - wordStart tword
  play $ staticFrame dur $ position $ outline svg
 where
  position = translate (-screenWidth / 2) (-screenHeight / 2)
  outline txt = mkGroup
    [ withStrokeWidth (defaultStrokeWidth * 10) $ withStrokeColor "white" txt
    , withStrokeWidth 0 txt
    ]

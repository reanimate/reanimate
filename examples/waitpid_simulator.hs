#!/usr/bin/env stack
-- stack runghc --package reanimate
module Main (main) where
 
import           Reanimate
import           Reanimate.Builtin.Documentation
import           Reanimate.Transition
import           Reanimate.Svg
import           Reanimate.Scene
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Graphics.SvgTree                hiding (Point, Text, height)
import           Text.Printf


customDuration = 3
data Point = Point Double Double
data Segment = Segment Point Point

-- Helper functions creating an Animation from two coordinates
animatePart :: Segment -> Animation
animatePart (Segment (Point startx starty) (Point endx endy)) = setDuration customDuration $
  animate $ \t ->
    partialSvg t $ pathify $ mkLine (startx, starty) (endx, endy) 

-- Helper function to create coordinates for a specific line
splitSegments :: Segment -> Int -> [Segment]
splitSegments seg numSegs =
              let firstSeg = getFirstSeg seg numSegs
              in take numSegs (iterate shiftSeg firstSeg)

-- Helper function to create the first smaller segment in a large segment
getFirstSeg :: Segment -> Int -> Segment
getFirstSeg (Segment (Point startx starty) (Point endx endy)) numSegs
             = Segment (Point startx starty) (Point ( startx + ((endx - startx)/(fromIntegral numSegs))) ( starty + ((endy - starty)/(fromIntegral numSegs))))

-- Helper function to shift a segment to the right by the size of itself
shiftSeg :: Segment -> Segment
shiftSeg (Segment (Point startx starty) (Point endx endy))
       = Segment (Point endx endy) (Point (endx + (endx-startx)) (endy + (endy-starty)))

-- Helper function to animate the line segment that represents a fork
animateFork :: Segment -> Animation
animateFork (Segment (Point startx starty) (Point endx endy))
       = staticFrame 0.1 (mkLine (startx,starty) (endx, endy))

-- Helper function to label a proccess with a pid
labelPids :: Segment -> Int -> SVG
labelPids (Segment (Point startx starty) (Point endx endy)) i = mkGroup
           [translate (startx - 0.75) (starty) $ scale 0.25 $ outlineText $T.pack("pid: " ++ (show i))]

-- Helper function to position a line of text
mkTextLabel:: [Char] -> Point -> SVG
mkTextLabel txt (Point startx starty) = mkGroup
           [translate (startx) (starty) $ scale 0.2 $ outlineText $ T.pack(txt)]

-- Helper function to label a printf() call
mkPrintLabels :: [Point] -> [Char] -> [Animation]
mkPrintLabels pointList txt =
          map slowAnimation ( map oFadeIn (map (\p -> mkTextLabel ("printf(" ++ (show txt) ++ ")" ) p) pointList))

mkPrintLabelFromPnt :: Point -> [Char] -> Animation
mkPrintLabelFromPnt pnt txt = 
                             oFadeIn $ mkTextLabel ("printf(" ++ (show txt) ++ ")" ) pnt
-- Helper function to speed up an animation
speedUpAnimation :: Animation -> Animation
speedUpAnimation anim = adjustDuration (*0.5) anim 
 
-- Helper function to slow down an animation
slowAnimation :: Animation -> Animation
slowAnimation anim = adjustDuration (*1.5) anim
 
-- Helper function to label a fork() call and its return value
mkForkLabel :: Point -> Int -> Animation
mkForkLabel point val = oFadeIn (mkTextLabel ("fork() = " ++ (show val) ) point)

-- Helper function that creates waitpid() call label
mkWaitpidLabel :: Point -> Animation
mkWaitpidLabel pnt = oFadeIn $ mkTextLabel "waitpid(p, NULL, 0)" pnt

-- Helper function to create a text label
createText :: Text -> SVG
createText txt = mkGroup
             [ center
             $ withStrokeColorPixel rtfdBackgroundColor
             $ withStrokeWidth (0)
             $ withFillOpacity 0
             $ latex txt
             , center $ latex txt ]

-- Helper function that creates an SVG of outlined text
outlineText :: Text -> SVG
outlineText txt = mkGroup
        [ center
        $ withStrokeColorPixel rtfdBackgroundColor
        $ withStrokeWidth (defaultStrokeWidth * 8)
        $ withFillOpacity 0
        $ latex txt
        , center $ latex txt
        ]

main :: IO ()
main = reanimate 
       $ docEnv
       $ ((pidLabelAnimations !! 0) `parA` (parentAnimations !! 0)) 
        `andThen` ((forkLabelAnims !! 0) `parA` (forkAnimation)) 
        `andThen` (forkLabelAnims !! 1) 
        `andThen` ((pidLabelAnimations !! 1) `parA` (childAnimations !! 0) `parA` waitpidLabelAnim) 
        `andThen` (childPrintfLabelAnim)
        `andThen` (parentAnimations !! 1) 
        `andThen` (parentPrintfLabelAnim)

--Lists of segments that represent each proccess 
parentSegments = splitSegments (Segment (Point (-6) 0) (Point 6 0)) 2
childSegments = splitSegments (Segment (Point 0 (-3)) (Point 6 (-3))) 1
forkSegment = Segment (Point 0 0) (Point 0 (-3))

--Lists of animations for each line 
parentAnimations = map animatePart parentSegments
childAnimations = map animatePart childSegments
forkAnimation = animateFork forkSegment

--List of animations to label each proccess with its PID
pidLabelAnimations = [speedUpAnimation (oFadeIn (labelPids (parentSegments !! 0) 1000)), speedUpAnimation (oFadeIn (labelPids (childSegments !! 0) 1001))]

--List of animations to label the fork() calls
forkLabelAnims = [mkForkLabel (Point (0) 0.25) 1001, mkForkLabel (Point (0) (-2.75)) 0]

-- Animation to label the waitpid() call
waitpidLabelAnim = mkWaitpidLabel (Point (0.75) (0.5))

-- Animation to label printf() 
parentPrintfLabelAnim = mkPrintLabelFromPnt (Point (5) (0.25)) "I'm the parent, and I'm done waiting!"
childPrintfLabelAnim = mkPrintLabelFromPnt (Point (5) (-2.75)) "I'm the child!"

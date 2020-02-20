module Reanimate.Chiphunk
  ( simulate
  , BodyStore
  , newBodyStore
  , addToBodyStore
  , spaceFreeRecursive
  , polyShapesToBody
  , polygonsToBody
  ) where

import           Chiphunk.Low
import           Control.Monad
import           Data.IORef
import           Data.Map            (Map)
import qualified Data.Map            as Map
import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as V
import           Foreign.Ptr
import           Graphics.SvgTree    (Tree)
import           Linear.V2 (V2(..))
import           Reanimate.Animation
import           Reanimate.PolyShape
import           Reanimate.Svg.Constructors

type BodyStore = IORef (Map WordPtr Tree)

newBodyStore :: IO BodyStore
newBodyStore = newIORef Map.empty

addToBodyStore :: BodyStore -> Body -> Tree -> IO ()
addToBodyStore store body svg = do
  key <- atomicModifyIORef' store $ \m ->
          case Map.maxViewWithKey m of
            Nothing -> (Map.singleton 1 svg, 1)
            Just ((maxKey,_),_) ->
              (Map.insert (maxKey+1) svg m, maxKey+1)
  bodyUserData body $= wordPtrToPtr key

renderBodyStore :: Space -> BodyStore -> IO Tree
renderBodyStore space store = do
  m <- readIORef store
  lst <- newIORef []
  spaceEachBody space (\body _dat -> do
    key <- get (bodyUserData body)
    case Map.lookup (ptrToWordPtr key) m of
      Nothing -> putStrLn "Body doesn't have an associated SVG"
      Just svg -> do
        Vect posX posY <- get $ bodyPosition body
        angle <- get $ bodyAngle body
        let bodySvg =
              translate posX posY $
              rotate (angle/pi*180)
              svg
        modifyIORef lst (bodySvg:)
    ) nullPtr
  result <- readIORef lst
  return $ mkGroup result


simulate :: Space -> BodyStore -> Double -> Int -> Double -> IO Animation
simulate space store fps stepsPerFrame dur = do
  let timeStep = 1/(fps*fromIntegral stepsPerFrame)
      frames = round (dur * fps)
  v <- V.new frames
  forM_ [0..frames-1] $ \nth -> do
    svg <- renderBodyStore space store
    V.write v nth svg
    replicateM_ stepsPerFrame $ spaceStep space timeStep
  frozen <- V.unsafeFreeze v
  return $ mkAnimation dur $ \t ->
    let key = round (t * fromIntegral (frames-1))
    in frozen V.! key

polyShapesToBody :: Space -> [PolyShape] -> IO Body
polyShapesToBody space poly =
    polygonsToBody space (map (map toVect) $ plDecompose poly)
  where
    toVect (V2 x y) = Vect x y

polygonsToBody :: Space -> [[Vect]] -> IO Body
polygonsToBody space polygons = do
  plBody <- bodyNew 0 0
  spaceAddBody space plBody

  forM_ polygons $ \vects -> do
    polyShape <- polyShapeNewRaw plBody vects 0.00
    shapeDensity polyShape $= 1
    spaceAddShape space polyShape
    shapeFriction polyShape $= 0.7
    shapeElasticity polyShape $= 0.5
  return plBody

spaceFreeRecursive :: Space -> IO ()
spaceFreeRecursive space = do
  spaceEachBody space (\body _ -> bodyFree body) nullPtr
  spaceEachShape space (\shape _ -> shapeFree shape) nullPtr
  spaceFree space

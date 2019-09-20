module Reanimate.Chiphunk
  ( simulate
  , BodyStore
  , newBodyStore
  , addToBodyStore
  ) where

import           Chiphunk.Low
import           Control.Monad
import           Data.IORef
import           Data.Map            (Map)
import qualified Data.Map            as Map
import           Data.Vector.Mutable (MVector)
import qualified Data.Vector.Mutable as V
import qualified Data.Vector as V
import           Foreign.Ptr
import           Graphics.SvgTree    (Tree)
import           Reanimate.Monad
import           Reanimate.Svg
import           Reanimate.Signal
import           Text.Printf

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
        vel <- get $ bodyVelocity body
        angle <- get $ bodyAngle body
        let bodySvg =
              translate posX posY $
              rotate (angle/pi*180) $
              svg
        modifyIORef lst $ (bodySvg:)
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
  return $ mkAnimation dur $ do
    t <- getSignal signalLinear
    let key = round (t * fromIntegral frames)
    emit $ frozen V.! key
{-
test :: IO ()
test = do
  bodyStore <- newBodyStore
  let gravity = Vect 0 (-100)

  -- Create an empty space.
  space <- spaceNew
  spaceGravity space $= gravity

  -- Add a static line segment shape for the ground.
  -- We'll make it slightly tilted so the ball will roll off.
  -- We attach it to a static body to tell Chipmunk it shouldn't be movable.
  static <- get $ spaceStaticBody space
  ground <- segmentShapeNew static (Vect (-20) 5) (Vect 20 (-5)) 0
  shapeFriction ground $= 1
  spaceAddShape space ground

  -- Now let's make a ball that falls onto the line and rolls off.
  -- First we need to make a cpBody to hold the physical properties of the object.
  -- These include the mass, position, velocity, angle, etc. of the object.
  -- Then we attach collision shapes to the Body to give it a size and shape.

  let radius = 5
  let mass = 1

  -- The moment of inertia is like mass for rotation
  -- Use the momentFor* functions to help you approximate it.
  let moment = momentForCircle mass 0 radius (Vect 0 0)

  -- The spaceAdd* functions return the thing that you are adding.
  ballBody <- bodyNew mass moment
  spaceAddBody space ballBody
  bodyPosition ballBody $= Vect 0 15

  -- Now we create the collision shape for the ball.
  -- You can create multiple collision shapes that point to the same body.
  -- They will all be attached to the body and move around to follow it.
  ballShape <- circleShapeNew ballBody radius (Vect 0 0)
  spaceAddShape space ballShape
  shapeFriction ballShape $= 0.7

  addToBodyStore bodyStore ballBody (mkGroup [])

  ani <- simulate space bodyStore 2

  putStrLn (renderTree $ frameAt 1 ani)

  -- Now that it's all set up, we simulate all the objects in the space by
  -- stepping forward through time in small increments called steps.
  -- It is *highly* recommended to use a fixed size time step.
  -- let timeStep = 1/60
  -- runFor 2 timeStep $ \time -> do
  --   pos <- get $ bodyPosition ballBody
  --   vel <- get $ bodyVelocity ballBody
  --   putStrLn . renderTree =<< renderBodyStore space bodyStore
  --   -- printf "Time is %4.2f. ballBody is at (%6.2f, %6.2f), it's velocity is (%6.2f, %6.2f).\n"
  --   --        time (vX pos) (vY pos) (vX vel) (vY vel)
  --
  --   spaceStep space timeStep

  shapeFree ballShape
  bodyFree ballBody
  shapeFree ground
  spaceFree space
  where
    runFor time step inner = go time
      where
        go time'
          | time' <= 0 = pure ()
          | otherwise  = inner (time - time') *> go (time' - step)
-}

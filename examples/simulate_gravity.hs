#!/usr/bin/env stack
-- stack runghc --package reanimate
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main (main) where

import           Chiphunk.Low
import           Reanimate.Chiphunk
import           Reanimate
import           System.IO.Unsafe


test :: Animation
test = unsafePerformIO $ do
  bodyStore <- newBodyStore
  let gravity = Vect 0 (-10)

  -- Create an empty space.
  space <- spaceNew
  spaceGravity space $= gravity

  -- Add a static line segment shape for the ground.
  -- We'll make it slightly tilted so the ball will roll off.
  -- We attach it to a static body to tell Chipmunk it shouldn't be movable.
  static <- get $ spaceStaticBody space
  ground <- segmentShapeNew static
    (Vect (-screenWidth/2) 0)
    (Vect (screenWidth/2) (-screenHeight/2)) 0
  shapeFriction ground $= 1
  spaceAddShape space ground

  -- Now let's make a ball that falls onto the line and rolls off.
  -- First we need to make a cpBody to hold the physical properties of the object.
  -- These include the mass, position, velocity, angle, etc. of the object.
  -- Then we attach collision shapes to the Body to give it a size and shape.

  let radius = 1
  let mass = 1

  -- The moment of inertia is like mass for rotation
  -- Use the momentFor* functions to help you approximate it.
  let moment = momentForCircle mass 0 radius (Vect 0 0)

  -- The spaceAdd* functions return the thing that you are adding.
  ballBody <- bodyNew mass moment
  spaceAddBody space ballBody
  bodyPosition ballBody $= Vect 0 (screenHeight/2)

  -- Now we create the collision shape for the ball.
  -- You can create multiple collision shapes that point to the same body.
  -- They will all be attached to the body and move around to follow it.
  ballShape <- circleShapeNew ballBody radius (Vect 0 0)
  spaceAddShape space ballShape
  shapeFriction ballShape $= 0.7

  addToBodyStore bodyStore ballBody $
    withFillColor "white" $
    mkGroup
      [ mkCircle radius
      , withStrokeColor "black" $
        mkLine (0, 0) (0, radius) ]

  ani <- simulate space bodyStore 60 3 4

  shapeFree ballShape
  bodyFree ballBody
  shapeFree ground
  spaceFree space
  return ani


main :: IO ()
main = reanimate $ bg `sim` line `sim` test
  where
    bg = animate $ const $ mkBackground "black"
    line = animate $ const $ withStrokeColor "white" $
      withStrokeWidth 0.01 $
      mkLine (-screenWidth/2, 0)
             (screenWidth/2, -screenHeight/2)

module Asteroid where

import Constants
import Data.Vec2 exposing (..)
import Random exposing (Seed)
import RandomHelpers exposing (..)
import Trampoline
import Vec2Helpers exposing (..)


type Size
  = Big
  | Medium
  | Small

type alias Asteroid =
  { size : Size
  , position : Vec2
  , momentum : Vec2
  }


newAsteroid : Seed -> (Asteroid, Seed)
newAsteroid seed =
  let
    (boundsMin, boundsMax) = Constants.gameBounds
    (side, seed') = randomInt 0 4 seed
    (position, seed'') =
      if | side == 0 -> randomVec2Y seed' boundsMin.x boundsMin.y boundsMax.y
         | side == 1 -> randomVec2X seed' boundsMin.y boundsMin.x boundsMax.x
         | side == 2 -> randomVec2Y seed' boundsMax.x boundsMin.y boundsMax.y
         | otherwise -> randomVec2X seed' boundsMax.y boundsMin.x boundsMax.x
    (momentum, seed''') = newMomentum Big seed''
    asteroid' =
      { size = Big
      , position = position
      , momentum = momentum
      }
  in
    (asteroid', seed''')


tickAsteroid : Asteroid -> Asteroid
tickAsteroid asteroid =
  { asteroid
  | position <-
      addVec asteroid.position asteroid.momentum
      |> wrapVec2 Constants.gameBounds
  }


newMomentum : Size -> Seed -> (Vec2, Seed)
newMomentum size seed =
  case size of
    Big -> randomVec2 seed -Constants.asteroidSpeedBig Constants.asteroidSpeedBig
    Medium -> randomVec2 seed -Constants.asteroidSpeedMedium Constants.asteroidSpeedMedium
    Small -> randomVec2 seed -Constants.asteroidSpeedSmall Constants.asteroidSpeedSmall


destroyAsteroid : Asteroid -> Seed -> Maybe (Asteroid, Asteroid, Seed)
destroyAsteroid asteroid seed =
  case asteroid.size of
    Big -> Just (splitAsteroid asteroid Medium seed)
    Medium -> Just (splitAsteroid asteroid Small seed)
    Small -> Nothing


splitAsteroid : Asteroid -> Size -> Seed -> (Asteroid, Asteroid, Seed)
splitAsteroid asteroid size seed =
  let
    newAsteroid = { asteroid | size <- size }
    (momentumA, seed') = newMomentum size seed
    (momentumB, seed'') = newMomentum size seed'
  in
    ( { newAsteroid | momentum <- momentumA } |> tickAsteroid
    , { newAsteroid | momentum <- momentumB } |> tickAsteroid
    , seed''
    )


splitAsteroids : List Asteroid -> Seed -> (List Asteroid, Seed)
splitAsteroids asteroids seed =
  (asteroids, seed)


asteroidSize : Asteroid -> Float
asteroidSize asteroid =
  case asteroid.size of
    Big -> Constants.asteroidSizeBig
    Medium -> Constants.asteroidSizeMedium
    Small -> Constants.asteroidSizeSmall

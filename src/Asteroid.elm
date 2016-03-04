module Asteroid
  ( Asteroid
  , Size (..)
  , Kind (..)
  , Status (..)
  , newAsteroid
  , introAsteroid
  , tickAsteroid
  , destroyAsteroid
  , asteroidSize
  , asteroidScore
  , asteroidSound
  ) where

import Constants
import Vec2 exposing (..)
import Random exposing (..)
import RandomHelpers exposing (..)


type Size
  = Big
  | Medium
  | Small


type Kind
  = A
  | B
  | C

type Status
  = Active
  | Destroyed


type alias Asteroid =
  { sizeClass : Size
  , kind : Kind
  , status : Status
  , position : Vec2
  , momentum : Vec2
  , size : Float
  , angle : Float
  }


defaultAsteroid : Asteroid
defaultAsteroid =
  { sizeClass = Big
  , size = asteroidSize Big
  , kind = A
  , status = Active
  , position = origin
  , momentum = origin
  , angle = 0
  }


newAsteroid : Generator Asteroid
newAsteroid =
  map
    (\position -> { defaultAsteroid | position = position })
    positionOnSideGenerator
  `andThen` asteroidPropertiesRandomizer


introAsteroid : Generator Asteroid
introAsteroid =
  map2
    (\size position ->
      { defaultAsteroid
      | sizeClass = size
      , size = asteroidSize size
      , position = position
      }
    )
    sizeGenerator
    (randomVec2InBounds Constants.gameBounds)
  `andThen` asteroidPropertiesRandomizer


asteroidPropertiesRandomizer : Asteroid -> Generator Asteroid
asteroidPropertiesRandomizer asteroid =
  map2 (\kind angle ->
      { asteroid
      | kind = kind
      , angle = angle
      }
    )
    kindGenerator
    (Random.float 0 (2 * pi))
  `andThen` randomMomentumGenerator


kindGenerator : Generator Kind
kindGenerator =
  map
    (\kind ->
      case kind of
        0 -> A
        1 -> B
        _ -> C
    )
    (Random.int 0 3)


sizeGenerator : Generator Size
sizeGenerator =
  map
    (\kind ->
      case kind of
        0 -> Big
        1 -> Medium
        _ -> Small
    )
    (Random.int 0 3)


randomMomentumGenerator : Asteroid -> Generator Asteroid
randomMomentumGenerator asteroid =
  let
    momentumGenerator =
      case asteroid.sizeClass of
        Big -> randomVec2 -Constants.asteroidSpeedBig Constants.asteroidSpeedBig
        Medium -> randomVec2 -Constants.asteroidSpeedMedium Constants.asteroidSpeedMedium
        Small -> randomVec2 -Constants.asteroidSpeedSmall Constants.asteroidSpeedSmall
  in
    map (applyMomentum asteroid) momentumGenerator


applyMomentum : Asteroid -> Vec2 -> Asteroid
applyMomentum asteroid momentum =
  let
    mag = length momentum
    momentum' =
      if mag < Constants.asteroidSpeedMin then
        scale (Constants.asteroidSpeedMin / mag) momentum
      else
        momentum
  in
    {asteroid | momentum = momentum'}


positionOnSideGenerator : Generator Vec2
positionOnSideGenerator =
  let
    (boundsMin, boundsMax) = Constants.gameBounds
  in
    (Random.int 0 4) `andThen` \side ->
      case side of
        0 -> randomVec2Y boundsMin.x boundsMin.y boundsMax.y
        1 -> randomVec2X boundsMin.y boundsMin.x boundsMax.x
        2 -> randomVec2Y boundsMax.x boundsMin.y boundsMax.y
        _ -> randomVec2X boundsMax.y boundsMin.x boundsMax.x


tickAsteroid : Asteroid -> Asteroid
tickAsteroid asteroid =
  { asteroid
  | position =
      add asteroid.position asteroid.momentum
      |> wrapVec2 Constants.gameBounds
  }


destroyAsteroid : Asteroid -> Seed -> Maybe (Asteroid, Asteroid, Seed)
destroyAsteroid asteroid seed =
  case asteroid.sizeClass of
    Big -> Just (splitAsteroid asteroid Medium seed)
    Medium -> Just (splitAsteroid asteroid Small seed)
    Small -> Nothing


splitAsteroid : Asteroid -> Size -> Seed -> (Asteroid, Asteroid, Seed)
splitAsteroid asteroid size seed =
  let
    generator =
      { asteroid
      | sizeClass = size
      , size = asteroidSize size
      , status = Active
      }
      |> asteroidPropertiesRandomizer
    ((a, b), seed') = generate (pair generator generator) seed
  in
    ( a |> tickAsteroid
    , b |> tickAsteroid
    , seed'
    )


splitAsteroids : List Asteroid -> Seed -> (List Asteroid, Seed)
splitAsteroids asteroids seed =
  (asteroids, seed)


asteroidSize : Size -> Float
asteroidSize size =
  case size of
    Big -> Constants.asteroidSizeBig
    Medium -> Constants.asteroidSizeMedium
    Small -> Constants.asteroidSizeSmall


asteroidScore : Asteroid -> Int
asteroidScore asteroid =
  case asteroid.sizeClass of
    Big -> Constants.asteroidScoreBig
    Medium -> Constants.asteroidScoreMedium
    Small -> Constants.asteroidScoreSmall


asteroidSound : Asteroid -> String
asteroidSound asteroid =
  case asteroid.sizeClass of
    Big -> Constants.asteroidSoundBig
    Medium -> Constants.asteroidSoundMedium
    Small -> Constants.asteroidSoundSmall

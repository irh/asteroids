module Saucer
  ( Saucer
  , defaultSaucer
  , newSaucer
  , moveSaucer
  , tickSaucer
  , maybeFireShot
  , saucerScore
  , saucerSize
  , saucerSizeForCollisions
  ) where

import Constants
import Data.Vec2 exposing (..)
import Debug
import Random exposing (Seed)
import RandomHelpers exposing (..)
import Ship exposing (Ship)
import Shot exposing (Shot)
import Vec2Helpers exposing (wrapVec2, randomVec2InBounds, angleBetween)


type Size
  = Big
  | Small

type alias Saucer =
  { size : Size
  , position : Vec2
  , momentum : Vec2
  , tickCount : Int
  , tickNextDirectionChange : Int
  , shotAccuracy : Float
  }


defaultSaucer : Saucer
defaultSaucer =
  { size = Small
  , tickCount = 0
  , position = origin
  , momentum = origin
  , tickNextDirectionChange = -1
  , shotAccuracy = 0
  }


newSaucer : Int -> Seed -> (Saucer, Seed)
newSaucer score seed =
  let
    (select, seed') = randomInt 0 2 seed
    size =
      if score >= 10000 || select == 0 then Small else Big
    difficulty = min 1.0 ((toFloat score) / (toFloat Constants.maxDifficultyScore))
    shotAccuracy = interpolate Constants.saucerShotAccuracyRange difficulty
    saucer =
      { defaultSaucer
      | size <- size
      , shotAccuracy <- shotAccuracy
      }
  in
    (saucer, seed')
    |> randomXPosition
    |> randomYPosition
    |> randomYSpeed
    |> scheduleDirectionChange


interpolate : (number, number) -> number -> number
interpolate (min, max) x =
  min + (max - min) * x


randomXPosition : (Saucer, Seed) -> (Saucer, Seed)
randomXPosition (saucer, seed) =
  let
    (dirX, seed') = randomInt 0 2 seed
    (positionX, speedX) =
      if dirX == 0 then
        (Constants.gameBoundsMaxX, -Constants.saucerSpeedX)
      else
        (Constants.gameBoundsMinX, Constants.saucerSpeedX)
  in
    ( { saucer
      | position <- { x = positionX, y = saucer.position.y }
      , momentum <- { x = speedX, y = saucer.momentum.y }
      }
    , seed'
    )


randomYPosition : (Saucer, Seed) -> (Saucer, Seed)
randomYPosition (saucer, seed) =
  let
    (positionY, seed') =
      randomFloat Constants.gameBoundsMinY Constants.gameBoundsMaxY seed
  in
    ( { saucer | position <- { x = saucer.position.x, y = positionY } }
    , seed'
    )


randomYSpeed : (Saucer, Seed) -> (Saucer, Seed)
randomYSpeed (saucer, seed) =
  let
    (dirY, seed') = randomInt 0 3 seed
    speedY =
      if | dirY == 0 -> -Constants.saucerSpeedY
         | dirY == 1 -> 0
         | otherwise -> Constants.saucerSpeedY
  in
    ( { saucer | momentum <- { x = saucer.momentum.x, y = speedY } }
    , seed'
    )


scheduleDirectionChange : (Saucer, Seed) -> (Saucer, Seed)
scheduleDirectionChange (saucer, seed) =
  let (change, seed') =
    randomInt Constants.saucerDirectionTicksMin Constants.saucerDirectionTicksMax seed
  in
    ( { saucer | tickNextDirectionChange <- saucer.tickCount + change }
    , seed'
    )


moveSaucer : Maybe Saucer -> Maybe Saucer
moveSaucer maybeSaucer =
  case maybeSaucer of
    Nothing -> Nothing
    Just saucer ->
      let
        position = addVec saucer.position saucer.momentum
        wrappedY =
          if | position.y > Constants.gameBoundsMaxY -> Constants.gameBoundsMinY
             | position.y < Constants.gameBoundsMinY -> Constants.gameBoundsMaxY
             | otherwise -> position.y
        outOfBounds =
          position.x < Constants.gameBoundsMinX || position.x > Constants.gameBoundsMaxX
      in
        if outOfBounds then Nothing else Just { saucer | position <- position }


tickSaucer : Saucer -> Seed -> (Saucer, Seed)
tickSaucer saucer seed =
  { saucer
  | tickCount <- saucer.tickCount + 1
  }
  |> changeDirection seed


changeDirection : Seed -> Saucer -> (Saucer, Seed)
changeDirection seed saucer =
  if saucer.tickCount == saucer.tickNextDirectionChange then
    let
      (dir, seed') = randomInt 0 2 seed
      newY =
        ( if | saucer.momentum.y > 0 -> if dir == 0 then -1 else 0
             | saucer.momentum.y < 0 -> if dir == 0 then 1 else 0
             | otherwise -> if dir == 0 then 1 else -1
        ) * Constants.saucerSpeedY
      saucer' = { saucer
        | momentum <- { x = saucer.momentum.x, y = newY }
        }
    in
      (saucer', seed')
      |> scheduleDirectionChange
  else
    (saucer, seed)


maybeFireShot : Saucer -> Maybe Ship -> Seed -> (Maybe Shot, Seed)
maybeFireShot saucer ship seed =
  if saucer.tickCount % Constants.saucerShotTicks == 0 then
    let
      (angle, seed') = shotAngle saucer ship seed
      shotOffset = (saucerSize saucer) / 2 + Constants.shotSize
      shotPosition =
        rotVec angle { x = 0.0, y = shotOffset }
        |> addVec saucer.position
    in
      (Just (Shot.newShot shotPosition angle), seed')
  else
    (Nothing, seed)


shotAngle : Saucer -> Maybe Ship -> Seed -> (Float, Seed)
shotAngle saucer maybeShip seed =
  case saucer.size of
    Big -> randomAngle seed
    Small ->
      case maybeShip of
        Just ship ->
          case ship.status of
            Ship.Dead -> randomAngle seed
            Ship.Hyperspace -> randomAngle seed
            _ ->
              let
                angle = pi - angleBetween ship.position saucer.position
                spread = pi * (1.0 - saucer.shotAccuracy)
                (jitter, seed') = randomFloat -spread spread seed
              in
                (angle + jitter, seed')
        Nothing -> randomAngle seed


saucerScore : Saucer -> Int
saucerScore saucer =
  case saucer.size of
    Big -> Constants.saucerScoreBig
    Small -> Constants.saucerScoreSmall


saucerSize : Saucer -> Float
saucerSize saucer =
  case saucer.size of
    Big -> Constants.saucerSizeBig
    Small -> Constants.saucerSizeSmall


saucerSizeForCollisions : Saucer -> Float
saucerSizeForCollisions saucer =
  Constants.saucerSizeCollisionRatio
    * case saucer.size of
        Big -> Constants.saucerSizeBig
        Small -> Constants.saucerSizeSmall

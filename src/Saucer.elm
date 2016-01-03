module Saucer
  ( Saucer
  , defaultSaucer
  , newSaucer
  , moveSaucer
  , tickSaucer
  , maybeFireShot
  , saucerScore
  , saucerSizeForView
  ) where

import Constants
import Vec2 exposing (..)
import Random exposing (Seed, generate)
import RandomHelpers exposing (..)
import Ship exposing (Ship)
import Shot exposing (Shot)


type Type
  = Big
  | Small

type alias Saucer =
  { saucerType : Type
  , position : Vec2
  , momentum : Vec2
  , size : Float
  , tickCount : Int
  , tickNextDirectionChange : Int
  , shotAccuracy : Float
  }


defaultSaucer : Saucer
defaultSaucer =
  { saucerType = Small
  , position = origin
  , momentum = origin
  , size = 0
  , tickCount = 0
  , tickNextDirectionChange = -1
  , shotAccuracy = 0
  }


newSaucer : Int -> Int -> Seed -> (Saucer, Seed)
newSaucer score saucerCount seed =
  let
    (select, seed') = randomInt 0 3 seed
    saucerType =
      if saucerCount <= Constants.initialBigSaucerCount then
        Big
      else if score >= 10000 then
        Small
      else if select == 0 then
        Small
      else
        Big
    size = Constants.saucerSizeCollisionRatio *
      if saucerType == Small then
        Constants.saucerSizeSmall
      else
        Constants.saucerSizeBig
    shotAccuracy =
      if saucerType == Small then
        let
          difficulty = min 1.0 ((toFloat score) / (toFloat Constants.maxDifficultyScore))
        in
          interpolate Constants.saucerShotAccuracyRange difficulty
      else 0
    saucer =
      { defaultSaucer
      | saucerType = saucerType
      , size = size
      , shotAccuracy = shotAccuracy
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
      | position = { x = positionX, y = saucer.position.y }
      , momentum = { x = speedX, y = saucer.momentum.y }
      }
    , seed'
    )


randomYPosition : (Saucer, Seed) -> (Saucer, Seed)
randomYPosition (saucer, seed) =
  let
    (positionY, seed') =
      randomFloat Constants.gameBoundsMinY Constants.gameBoundsMaxY seed
  in
    ( { saucer | position = { x = saucer.position.x, y = positionY } }
    , seed'
    )


randomYSpeed : (Saucer, Seed) -> (Saucer, Seed)
randomYSpeed (saucer, seed) =
  let
    (dirY, seed') = randomInt 0 3 seed
    speedY = case dirY of
      0 -> -Constants.saucerSpeedY
      1 -> 0
      _ -> Constants.saucerSpeedY
  in
    ( { saucer | momentum = { x = saucer.momentum.x, y = speedY } }
    , seed'
    )


scheduleDirectionChange : (Saucer, Seed) -> (Saucer, Seed)
scheduleDirectionChange (saucer, seed) =
  let (change, seed') =
    randomInt Constants.saucerDirectionTicksMin Constants.saucerDirectionTicksMax seed
  in
    ( { saucer | tickNextDirectionChange = saucer.tickCount + change }
    , seed'
    )


moveSaucer : Maybe Saucer -> Maybe Saucer
moveSaucer maybeSaucer =
  case maybeSaucer of
    Nothing -> Nothing
    Just saucer ->
      let
        position = add saucer.position saucer.momentum
        wrappedY =
          if position.y > Constants.gameBoundsMaxY then
            Constants.gameBoundsMinY
          else if position.y < Constants.gameBoundsMinY then
            Constants.gameBoundsMaxY
          else
            position.y
        positionWrapped = { position | y = wrappedY }
      in
        Just { saucer | position = positionWrapped }


tickSaucer : Saucer -> Seed -> (Saucer, Seed)
tickSaucer saucer seed =
  { saucer
  | tickCount = saucer.tickCount + 1
  }
  |> changeDirection seed


changeDirection : Seed -> Saucer -> (Saucer, Seed)
changeDirection seed saucer =
  if saucer.tickCount == saucer.tickNextDirectionChange then
    let
      (dir, seed') = randomInt 0 2 seed
      newY =
        ( if saucer.momentum.y > 0 then
            if dir == 0 then -1 else 0
          else if saucer.momentum.y < 0 then
            if dir == 0 then 1 else 0
          else
            (if dir == 0 then 1 else -1)
        ) * Constants.saucerSpeedY
      saucer' =
        { saucer
        | momentum = { x = saucer.momentum.x, y = newY }
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
      shotOffset = saucer.size + Constants.shotSize
      shotPosition =
        rotate angle { x = 0.0, y = shotOffset }
        |> add saucer.position
    in
      (Just (Shot.newShot shotPosition angle), seed')
  else
    (Nothing, seed)


shotAngle : Saucer -> Maybe Ship -> Seed -> (Float, Seed)
shotAngle saucer maybeShip seed =
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
  case saucer.saucerType of
    Big -> Constants.saucerScoreBig
    Small -> Constants.saucerScoreSmall


saucerSizeForView : Saucer -> Float
saucerSizeForView saucer =
  saucer.size / Constants.saucerSizeCollisionRatio

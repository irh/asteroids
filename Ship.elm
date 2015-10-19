module Ship
  ( Ship
  , Status (..)
  , defaultShip
  , newShip
  , invincibleShip
  , moveShip
  , killShip
  , tickShipState
  , goIntoHyperspace
  , fireShot
  ) where

import Constants
import Data.Vec2 exposing (..)
import Debug
import GameObject exposing (GameObject)
import KeyboardHelpers
import Random exposing (Seed)
import Shot exposing (Shot)
import Vec2Helpers exposing (wrapVec2, randomVec2InBounds)


type Status
  = Alive
  | Invincible
  | Hyperspace
  | Dead

type alias Ship =
  { status : Status
  , tickCount : Int
  , angle : Float
  , thrust : Bool
  , position : Vec2
  , momentum : Vec2
  , size : Float
  }

defaultShip : Ship
defaultShip =
  { status = Dead
  , tickCount = 0
  , position = origin
  , momentum = origin
  , size = Constants.shipSizeForCollisions
  , angle = 0.0
  , thrust = False
  }


newShip : Ship
newShip =
  { defaultShip
  | status <- Alive
  }


invincibleShip : Ship
invincibleShip =
  { defaultShip
  | status <- Invincible
  }


killShip : Ship -> Ship
killShip ship =
  { ship
  | status <- Dead
  , tickCount <- 0
  , thrust <- False
  , momentum <- scaleVec Constants.deadShipMomentumChange ship.momentum
  }


goIntoHyperspace : Maybe Ship -> Seed -> (Maybe Ship, Seed)
goIntoHyperspace maybeShip seed =
  case maybeShip of
    Nothing -> (Nothing, seed)
    Just ship ->
      case ship.status of
        Alive ->
          let
            (position, seed') = randomVec2InBounds seed Constants.gameBounds
            ship' =
              { ship
              | status <- Hyperspace
              , position <- position
              , momentum <- origin
              , tickCount <- 0
              }
          in (Just ship', seed')
        _ -> (Just ship, seed)


tickShipState : Maybe Ship -> Maybe Ship
tickShipState maybeShip =
  case maybeShip of
    Nothing -> Nothing
    Just ship ->
      let
        tickCount = ship.tickCount + 1
        ship' = { ship | tickCount <- tickCount }
      in
        case ship.status of
          Dead ->
            if tickCount < Constants.deadShipTicks then
              Just ship'
            else
              Nothing
          Invincible ->
            if tickCount < Constants.invincibleShipTicks then
              Just ship'
            else
              Just { ship' | status <- Alive }
          Hyperspace ->
            if tickCount < Constants.hyperspaceTicks then
              Just ship'
            else
              Just { ship' | status <- Alive }
          _ -> Just ship'


moveShip : Maybe Ship -> KeyboardHelpers.Arrows -> Maybe Ship
moveShip maybeShip arrows =
  case maybeShip of
    Nothing -> Nothing
    Just ship ->
      let ship' =
        case ship.status of
          Dead -> moveDeadShip ship
          _ -> moveLiveShip ship arrows
      in Just ship'


moveDeadShip : Ship -> Ship
moveDeadShip ship =
  { ship
  | position <- applyMomentum ship.position ship.momentum
  }


moveLiveShip : Ship -> KeyboardHelpers.Arrows -> Ship
moveLiveShip ship arrows =
  let
    angle =
      if | arrows.x > 0 -> ship.angle - Constants.turningSpeed
         | arrows.x < 0 -> ship.angle + Constants.turningSpeed
         | otherwise -> ship.angle
    thrust = arrows.y > 0
    momentum =
      if | thrust -> applyThrust ship.momentum angle
         | otherwise -> ship.momentum
      |> scaleVec Constants.spaceFriction
    position = applyMomentum ship.position momentum
  in
    { ship
    | position <- position
    , momentum <- momentum
    , angle <- angle
    , thrust <- thrust
    }


applyMomentum : Vec2 -> Vec2 -> Vec2
applyMomentum position momentum =
  addVec position momentum
  |> wrapVec2 Constants.gameBounds


applyThrust : Vec2 -> Float -> Vec2
applyThrust momentum angle =
  let
    momentum' = addVec momentum (rotVec angle Constants.thrust)
    momentumMagnitude = magnitude momentum'
  in
    if momentumMagnitude > Constants.maxMomentum then
      scaleVec (Constants.maxMomentum / momentumMagnitude) momentum'
    else
      momentum'


fireShot : Maybe Ship -> Maybe Shot
fireShot maybeShip =
  case maybeShip of
    Nothing -> Nothing
    Just ship ->
      case ship.status of
        Dead -> Nothing
        Hyperspace -> Nothing
        _ ->
          let
            shotOffset = ship.size + Constants.shotSize
            shotPosition = rotVec ship.angle { x = 0.0, y = shotOffset }
              |> addVec ship.position
          in
            Just (Shot.newShot shotPosition ship.angle)


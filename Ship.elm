module Ship
  ( Ship
  , Status (..)
  , defaultShip
  , invincibleShip
  , moveShip
  , killShip
  , tickShipState
  , goIntoHyperspace
  ) where

import Constants
import Data.Vec2 exposing (..)
import Debug
import KeyboardHelpers
import Random exposing (Seed)
import Vec2Helpers exposing (wrapVec2, randomVec2InBounds)


type Status
  = Alive
  | Invincible
  | Hyperspace
  | Dead

type alias Ship =
  { status : Status
  , tickCount : Int
  , position : Vec2
  , momentum : Vec2
  , angle : Float
  , thrust : Bool
  }


defaultShip : Ship
defaultShip =
  { status = Alive
  , tickCount = 0
  , position = origin
  , momentum = origin
  , angle = 0.0
  , thrust = False
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


goIntoHyperspace : Ship -> Seed -> (Ship, Seed)
goIntoHyperspace ship seed =
  let
    (position, seed') = randomVec2InBounds seed Constants.gameBounds
  in
    ( { ship
      | status <- Hyperspace
      , position <- position
      , momentum <- origin
      , tickCount <- 0
    }, seed')


tickShipState : Ship -> Maybe Ship
tickShipState ship =
  let
    tickCount = ship.tickCount + 1
    ship' = { ship | tickCount <- tickCount }
  in
    case ship.status of
      Dead ->
        if tickCount < Constants.deadShipTime then
          Just ship'
        else
          Nothing
      Invincible ->
        if tickCount < Constants.invincibleShipTime then
          Just ship'
        else
          Just { ship' | status <- Alive }
      Hyperspace ->
        if tickCount < Constants.hyperspaceTime then
          Just ship'
        else
          Just { ship' | status <- Alive }
      _ -> Just ship'


moveShip : Ship -> KeyboardHelpers.Arrows -> Ship
moveShip ship arrows =
  case ship.status of
    Dead -> moveDeadShip ship
    _ -> moveLiveShip ship arrows


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



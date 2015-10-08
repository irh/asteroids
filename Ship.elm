module Ship
  ( Ship
  , Status (..)
  , defaultShip
  , moveShip
  , killShip
  , tickShipState
  ) where

import Constants
import Debug
import KeyboardHelpers
import Data.Vec2 exposing (..)
import Vec2Helpers exposing (wrapVec2)


type Status
  = Alive
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


killShip : Ship -> Ship
killShip ship =
  { ship
  | status <- Dead
  , tickCount <- 0
  , thrust <- False
  , momentum <- scaleVec Constants.deadShipMomentumChange ship.momentum
  }


tickShipState : Ship -> Maybe Ship
tickShipState ship =
  let
    tickCount = ship.tickCount + 1
  in
    case ship.status of
      Dead ->
        if tickCount < Constants.deadShipTime then
          Just { ship | tickCount <- tickCount }
        else
          Nothing
      _ -> Just { ship | tickCount <- tickCount }


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


module Ship
  ( Ship
  , Status (..)
  , defaultShip
  , moveShip
  , killShip
  , tickDeadShip
  ) where

import Constants
import Debug
import KeyboardHelpers
import Data.Vec2 exposing (..)
import Vec2Helpers exposing (wrapVec2)


type Status
  = Alive
  | Dead Int

type alias Ship =
  { status : Status
  , position : Vec2
  , momentum : Vec2
  , angle : Float
  , thrust : Bool
  }


defaultShip : Ship
defaultShip =
  { status = Alive
  , position = origin
  , momentum = origin
  , angle = 0.0
  , thrust = False
  }


killShip : Ship -> Ship
killShip ship =
  { ship
  | status <- Dead 0
  , thrust <- False
  , momentum <- scaleVec Constants.deadShipMomentumChange ship.momentum
  }


moveShip : Ship -> KeyboardHelpers.Arrows -> Ship
moveShip ship arrows =
  let
    applyMomentum position momentum =
      addVec position momentum
      |> wrapVec2 Constants.gameBounds
  in
    case ship.status of
      Dead _ ->
        { ship
        | position <- applyMomentum ship.position ship.momentum
        }
      _ ->
        let
          angle' =
            if | arrows.x > 0 -> ship.angle - Constants.turningSpeed
               | arrows.x < 0 -> ship.angle + Constants.turningSpeed
               | otherwise -> ship.angle
          thrust' = arrows.y > 0
          momentum' =
            if | thrust' -> applyThrust ship.momentum angle'
               | otherwise -> ship.momentum
            |> scaleVec Constants.spaceFriction
          position' = applyMomentum ship.position momentum'
        in
          { ship
          | position <- position'
          , momentum <- momentum'
          , angle <- angle'
          , thrust <- thrust' && not ship.thrust
          }


applyThrust : Vec2 -> Float -> Vec2
applyThrust momentum angle =
  let
    momentum' = addVec momentum (rotVec angle Constants.thrust)
    magnitude' = magnitude momentum'
  in
    if magnitude' > Constants.maxMomentum then
      scaleVec (Constants.maxMomentum / magnitude') momentum'
    else
      momentum'


tickDeadShip : Ship -> Maybe Ship
tickDeadShip ship =
  case ship.status of
    Dead tickCount ->
      let
        tickCount' = tickCount + 1
      in
        if tickCount' < Constants.deadShipTime then
          Just { ship | status <- Dead tickCount' }
        else
          Nothing
    _ -> Just ship

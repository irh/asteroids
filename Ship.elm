module Ship
  ( Ship
  , defaultShip
  , moveShip
  ) where

import Constants
import Debug
import KeyboardHelpers
import Data.Vec2 exposing (..)
import Vec2Helpers exposing (wrapVec2)


type alias Ship =
  { position : Vec2
  , momentum : Vec2
  , angle : Float
  , thrust : Bool
  }


defaultShip : Ship
defaultShip =
  { position = origin
  , momentum = origin
  , angle = 0.0
  , thrust = False
  }


moveShip : Ship -> KeyboardHelpers.Arrows -> Ship
moveShip ship arrows =
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
    position' =
      addVec ship.position momentum'
      |> wrapVec2 Constants.gameBounds
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


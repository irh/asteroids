module Ship
  ( Ship
  , defaultShip
  , shipSize
  , moveShip
  ) where

import Constants
import Debug
import KeyboardHelpers
import Data.Vec2 exposing (..)
import Vec2Helpers exposing (wrapVec2)

shipSize = 0.03

turningSpeed = 0.1

thrust : Vec2
thrust = { x = 0, y = 0.00015 }

spaceFriction = 0.985
maxMomentum = 0.0125


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
      if | arrows.x > 0 -> ship.angle - turningSpeed
         | arrows.x < 0 -> ship.angle + turningSpeed
         | otherwise -> ship.angle
    thrust' = arrows.y > 0
    momentum' =
      if | thrust' -> applyThrust ship.momentum angle'
         | otherwise -> ship.momentum
      |> scaleVec spaceFriction
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
    momentum' = addVec momentum (rotVec angle thrust)
    magnitude' = magnitude momentum'
  in
    if magnitude' > maxMomentum then
      scaleVec (maxMomentum / magnitude') momentum'
    else
      momentum'


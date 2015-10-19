module Shot where

import Constants
import Data.Vec2 exposing (..)
import Vec2Helpers exposing (..)


type alias Shot =
  { position : Vec2
  , momentum : Vec2
  , size : Float
  , tickCount : Int
  }


newShot : Vec2 -> Float -> Shot
newShot position angle =
  { position = position
  , momentum = rotVec angle Constants.shotSpeed
  , size = Constants.shotSize
  , tickCount = 0
  }


tickShot : Shot -> Maybe Shot
tickShot shot =
  let
    tickCount' = shot.tickCount + 1
  in
    if tickCount' < Constants.shotLifetime then
      Just { shot
      | position <-
          addVec shot.position shot.momentum
          |> wrapVec2 Constants.gameBounds
      , tickCount <- tickCount'
      }
    else
      Nothing


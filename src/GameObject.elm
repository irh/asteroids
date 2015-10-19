module GameObject where

import Data.Vec2 exposing (Vec2)
import Vec2Helpers exposing (circlesOverlap)

type alias GameObject a =
  { a
  | position : Vec2
  , momentum : Vec2
  , size : Float
  }

collisionTest : GameObject (a) -> GameObject (b) -> Bool
collisionTest a b =
  circlesOverlap (a.position, a.size) (b.position, b.size)


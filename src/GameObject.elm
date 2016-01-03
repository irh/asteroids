module GameObject where

import Vec2 exposing (Vec2, circlesOverlap)

type alias GameObject a =
  { a
  | position : Vec2
  , momentum : Vec2
  , size : Float
  }

collisionTest : GameObject (a) -> GameObject (b) -> Bool
collisionTest a b =
  circlesOverlap (a.position, a.size) (b.position, b.size)


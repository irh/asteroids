module Point
  ( Point
  , defaultPoint
  , randomPoint
  , wrapPoint
  , asTuple
  ) where

import Random exposing (Seed)
import RandomHelpers exposing (randomFloat)

type alias Point = { x : Float, y : Float }


defaultPoint : Point
defaultPoint = { x = 0.0, y = 0.0 }


randomPoint : Seed -> Float -> Float -> (Point, Seed)
randomPoint seed min max =
  let
    (x, seed0) = randomFloat min max seed
    (y, seed1) = randomFloat min max seed0
  in
    ({x = x, y = y}, seed1)


wrapPoint : Point -> Float -> Float -> Point
wrapPoint input min max =
  { x = wrapFloat input.x min max
  , y = wrapFloat input.y min max
  }


wrapFloat : Float -> Float -> Float -> Float
wrapFloat input min max =
  let
    size = max - min
  in
    if input < min then
      input + size
    else if input >= max then
      input - size
    else
      input


asTuple : Point -> (Float, Float)
asTuple point = (point.x, point.y)

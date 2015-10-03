module Vec2Helpers where

import Data.Vec2 exposing (..)
import Random exposing (Seed)
import RandomHelpers exposing (randomFloat)


randomVec2 : Seed -> Float -> Float -> (Vec2, Seed)
randomVec2 seed min max =
  let
    (x, seed0) = randomFloat min max seed
    (y, seed1) = randomFloat min max seed0
  in
    ({x = x, y = y}, seed1)


wrapVec2 : (Vec2, Vec2) -> Vec2 -> Vec2
wrapVec2 (min, max) input =
  { x = wrapFloat input.x min.x max.x
  , y = wrapFloat input.y min.y max.y
  }


wrapFloat : Float -> Float -> Float -> Float
wrapFloat input min max =
  let
    size = max - min
  in
    if | input < min -> input + size
       | input >= max -> input - size
       | otherwise -> input


asTuple : Vec2 -> (Float, Float)
asTuple point = (point.x, point.y)

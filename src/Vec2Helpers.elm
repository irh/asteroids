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


randomVec2InBounds : Seed -> (Vec2, Vec2) -> (Vec2, Seed)
randomVec2InBounds seed (min, max) =
  let
    (x, seed0) = randomFloat min.x max.x seed
    (y, seed1) = randomFloat min.y max.y seed0
  in
    ({x = x, y = y}, seed1)


randomVec2X : Seed -> Float -> Float -> Float -> (Vec2, Seed)
randomVec2X seed y min max =
  let
    (x, seed') = randomFloat min max seed
  in
    ({x = x, y = y}, seed')


randomVec2Y : Seed -> Float -> Float -> Float -> (Vec2, Seed)
randomVec2Y seed x min max =
  let
    (y, seed') = randomFloat min max seed
  in
    ({x = x, y = y}, seed')


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


circlesOverlap : (Vec2, Float) -> (Vec2, Float) -> Bool
circlesOverlap (posA, radiusA) (posB, radiusB) =
  (distance posA posB) <= (radiusA + radiusB)


angleBetween : Vec2 -> Vec2 -> Float
angleBetween a b =
  let
    xDiff = b.x - a.x
    yDiff = b.y - a.y
  in
    atan2 xDiff yDiff

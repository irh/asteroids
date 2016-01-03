module Vec2 where

import Random exposing (Generator, float, map, map2)


type alias Vec2 = { x : Float, y : Float }


origin : Vec2
origin = { x = 0, y = 0 }


scale : Float -> Vec2 -> Vec2
scale a v =
  { x = v.x * a
  , y = v.y * a
  }


add : Vec2 -> Vec2 -> Vec2
add v1 v2 =
  { x = v1.x + v2.x
  , y = v1.y + v2.y
  }


subtract : Vec2 -> Vec2 -> Vec2
subtract v1 v2 =
  { x = v1.x - v2.x
  , y = v1.y - v2.y
  }


rotate : Float -> Vec2 -> Vec2
rotate a v =
  let
    cosa = cos a
    sina = sin a
  in
    { x = v.x * cosa - v.y * sina
    , y = v.x * sina + v.y * cosa
    }


length : Vec2 -> Float
length v =
  sqrt <| v.x * v.x + v.y * v.y


distance : Vec2 -> Vec2 -> Float
distance v1 v2 =
  length <| subtract v1 v2


randomVec2 : Float -> Float -> Generator Vec2
randomVec2 min max =
  map2 (\x y -> {x = x, y = y}) (float min max) (float min max)


randomVec2InBounds : (Vec2, Vec2) -> Generator Vec2
randomVec2InBounds (min, max) =
  map2 (\x y -> {x = x, y = y}) (float min.x max.x) (float min.y max.y)


randomVec2X : Float -> Float -> Float -> Generator Vec2
randomVec2X y min max =
  map (\x -> {x = x, y = y}) (float min max)


randomVec2Y : Float -> Float -> Float -> Generator Vec2
randomVec2Y x min max =
  map (\y -> {x = x, y = y}) (float min max)


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
    if input < min then
      input + size
    else if input >= max then
      input - size
    else
      input


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

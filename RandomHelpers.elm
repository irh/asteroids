module RandomHelpers
  ( randomFloat
  , randomInt
  , randomAngle
  ) where

import Random exposing (..)

randomInt : Int -> Int -> Seed -> (Int, Seed)
randomInt min max seed =
  generate (int min max) seed

randomFloat : Float -> Float -> Seed -> (Float, Seed)
randomFloat min max seed =
  generate (float min max) seed

randomAngle : Seed -> (Float, Seed)
randomAngle seed =
  randomFloat 0 (2 * pi) seed

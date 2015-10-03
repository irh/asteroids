module RandomHelpers
  ( randomFloat
  , randomInt
  ) where

import Random

randomInt : Int -> Int -> Random.Seed -> (Int, Random.Seed)
randomInt min max seed =
  Random.generate (Random.int min max) seed

randomFloat : Float -> Float -> Random.Seed -> (Float, Random.Seed)
randomFloat min max seed =
  Random.generate (Random.float min max) seed

module Explosion where

import Constants
import Data.Vec2 exposing (Vec2)
import Random exposing (Seed)
import RandomHelpers exposing (randomFloat)


type alias Explosion =
  { position : Vec2
  , angle : Float
  , tickCount : Int
  }


newExplosion : Vec2 -> Seed -> (Explosion, Seed)
newExplosion position seed =
  let
    (angle, seed') = randomFloat 0 (2 * pi) seed
  in
    ( { position = position
      , angle = angle
      , tickCount = 0
      }
    , seed'
    )


tickExplosion : Explosion -> Maybe Explosion
tickExplosion explosion =
  let
    tickCount = explosion.tickCount + 1
  in
    if tickCount < Constants.explosionLifetime then
      Just { explosion | tickCount <- tickCount }
    else
      Nothing

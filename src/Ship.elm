module Ship
  ( Ship
  , Status (..)
  , defaultShip
  , newShip
  , invincibleShip
  , moveShip
  , killShip
  , tickShipState
  , goIntoHyperspace
  , fireShot
  ) where

import Constants
import Vec2 exposing (..)
import KeyboardHelpers
import Random exposing (Seed, generate)
import Shot exposing (Shot)


type Status
  = Alive
  | Invincible
  | Hyperspace
  | Dead

type alias Ship =
  { status : Status
  , tickCount : Int
  , angle : Float
  , thrust : Bool
  , position : Vec2
  , momentum : Vec2
  , size : Float
  }

defaultShip : Ship
defaultShip =
  { status = Dead
  , tickCount = 0
  , position = origin
  , momentum = origin
  , size = Constants.shipSizeForCollisions
  , angle = 0.0
  , thrust = False
  }


newShip : Ship
newShip =
  { defaultShip
  | status = Alive
  }


invincibleShip : Ship
invincibleShip =
  { defaultShip
  | status = Invincible
  }


killShip : Ship -> Ship
killShip ship =
  { ship
  | status = Dead
  , tickCount = 0
  , thrust = False
  , momentum = scale Constants.deadShipMomentumChange ship.momentum
  }


goIntoHyperspace : Maybe Ship -> Seed -> (Maybe Ship, Seed)
goIntoHyperspace maybeShip seed =
  case maybeShip of
    Nothing -> (Nothing, seed)
    Just ship ->
      case ship.status of
        Alive ->
          let
            (position, seed') = generate (randomVec2InBounds Constants.gameBounds) seed
            ship' =
              { ship
              | status = Hyperspace
              , position = position
              , momentum = origin
              , tickCount = 0
              }
          in (Just ship', seed')
        _ -> (Just ship, seed)


tickShipState : Maybe Ship -> Maybe Ship
tickShipState maybeShip =
  case maybeShip of
    Nothing -> Nothing
    Just ship ->
      let
        tickCount = ship.tickCount + 1
        ship' = { ship | tickCount = tickCount }
      in
        case ship.status of
          Dead ->
            if tickCount < Constants.deadShipTicks then
              Just ship'
            else
              Nothing
          Invincible ->
            if tickCount < Constants.invincibleShipTicks then
              Just ship'
            else
              Just { ship' | status = Alive }
          Hyperspace ->
            if tickCount < Constants.hyperspaceTicks then
              Just ship'
            else
              Just { ship' | status = Alive }
          _ -> Just ship'


moveShip : Maybe Ship -> KeyboardHelpers.Arrows -> Maybe Ship
moveShip maybeShip arrows =
  case maybeShip of
    Nothing -> Nothing
    Just ship ->
      let ship' =
        case ship.status of
          Dead -> moveDeadShip ship
          _ -> moveLiveShip ship arrows
      in Just ship'


moveDeadShip : Ship -> Ship
moveDeadShip ship =
  { ship
  | position = applyMomentum ship.position ship.momentum
  }


moveLiveShip : Ship -> KeyboardHelpers.Arrows -> Ship
moveLiveShip ship arrows =
  let
    angle =
      if arrows.x > 0 then
        ship.angle - Constants.turningSpeed
      else if arrows.x < 0 then
        ship.angle + Constants.turningSpeed
      else
        ship.angle
    thrust = arrows.y > 0
    momentum =
      if thrust then
        applyThrust ship.momentum angle
      else
        ship.momentum
      |> scale Constants.spaceFriction
    position = applyMomentum ship.position momentum
  in
    { ship
    | position = position
    , momentum = momentum
    , angle = angle
    , thrust = thrust
    }


applyMomentum : Vec2 -> Vec2 -> Vec2
applyMomentum position momentum =
  add position momentum
  |> wrapVec2 Constants.gameBounds


applyThrust : Vec2 -> Float -> Vec2
applyThrust momentum angle =
  let
    momentum' = add momentum (rotate angle Constants.thrust)
    momentumMagnitude = length momentum'
  in
    if momentumMagnitude > Constants.maxMomentum then
      scale (Constants.maxMomentum / momentumMagnitude) momentum'
    else
      momentum'


fireShot : Maybe Ship -> Maybe Shot
fireShot maybeShip =
  case maybeShip of
    Nothing -> Nothing
    Just ship ->
      case ship.status of
        Dead -> Nothing
        Hyperspace -> Nothing
        _ ->
          let
            shotOffset = ship.size + Constants.shotSize
            shotPosition = rotate ship.angle { x = 0.0, y = shotOffset }
              |> add ship.position
          in
            Just (Shot.newShot shotPosition ship.angle)


module Game
  ( Update(..)
  , Mode(..)
  , Model
  , initialGame
  , updateGame
  ) where

import Constants
import Data.Vec2 exposing (..)
import Debug
import KeyboardHelpers
import Random exposing(Seed)
import Ship exposing (Ship)
import Shot exposing (..)
import Trampoline


type Update
  = Arrows KeyboardHelpers.Arrows
  | Wasd KeyboardHelpers.Arrows
  | Tick Float
  | Space Bool
  | Escape Bool
  | StartTime Float

type Mode
  = NewGame
  | Play
  | Pause
  | Dead Int
  | GameOver


type alias Model =
  { mode : Mode
  , arrows : KeyboardHelpers.Arrows
  , ship : Ship
  , shots : List Shot
  , score : Int
  , seed : Seed
  }


defaultGame : Model
defaultGame =
  { mode = Play
  , arrows = KeyboardHelpers.defaultArrows
  , ship = Ship.defaultShip
  , shots = []
  , score = 0
  , seed = Random.initialSeed 0
  }


initialGame : Update -> Model
initialGame input =
  case input of
    StartTime time ->
      { defaultGame | seed <- Random.initialSeed (round time) }
    _ -> defaultGame


newGame : Model -> Model
newGame game =
  { defaultGame | seed <- game.seed }


updateGame : Update -> Model -> Model
updateGame input game =
  case input of
    Arrows arrows -> { game | arrows <- arrows }
    Wasd wasd -> { game | arrows <- wasd }
    Tick _ -> tickGame game
    Space down -> if down then addShot game else game
    _ -> game


changeGameMode : Model -> Model
changeGameMode game =
  case game.mode of
    NewGame -> newGame game
    Play -> { game | mode <- Pause }
    Pause -> { game | mode <- Play }
    Dead _ -> game
    GameOver -> newGame game


tickGame : Model -> Model
tickGame game =
  Debug.watch "Game" <|
  case game.mode of
    Play -> tickPlay game
    _ -> game


tickPlay : Model -> Model
tickPlay game =
  let
    ship' = Ship.moveShip game.ship game.arrows
    shots' = List.filterMap Shot.tickShot game.shots
  in
    { game
    | ship <- ship'
    , shots <- shots'
    }


addShot : Model -> Model
addShot game =
  let
    shotOffset = Constants.shipSize / 2 + Constants.shotSize / 2
    shotPosition = rotVec game.ship.angle { x = 0.0, y = shotOffset }
      |> addVec game.ship.position
  in
    { game
    | shots <- (newShot shotPosition game.ship.angle) :: game.shots
    }


module Game
  ( Update(..)
  , Mode(..)
  , Model
  , initialGame
  , updateGame
  ) where

import Constants
import Debug
import KeyboardHelpers
import Random exposing(Seed)
import Ship exposing (..)
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
  , score : Int
  , seed : Seed
  }


defaultGame : Model
defaultGame =
  { mode = Play
  , arrows = KeyboardHelpers.defaultArrows
  , ship = defaultShip
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
    ship' = moveShip game.ship game.arrows
  in
    { game
    | ship <- ship'
    }


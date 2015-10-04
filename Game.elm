module Game
  ( Update(..)
  , Mode(..)
  , Model
  , initialGame
  , updateGame
  ) where

import Asteroid exposing (..)
import Constants
import Data.Vec2 exposing (..)
import Debug
import KeyboardHelpers
import Random exposing(Seed)
import Ship exposing (Ship)
import Shot exposing (..)
import Trampoline
import Vec2Helpers exposing (..)


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
  , asteroids : List Asteroid
  , score : Int
  , level : Int
  , seed : Seed
  }


defaultGame : Model
defaultGame =
  { mode = Play
  , arrows = KeyboardHelpers.defaultArrows
  , ship = Ship.defaultShip
  , shots = []
  , asteroids = []
  , score = 0
  , level = 0
  , seed = Random.initialSeed 0
  }


initialGame : Update -> Model
initialGame input =
  case input of
    StartTime time ->
      { defaultGame | seed <- Random.initialSeed (round time) }
      |> newLevel
    _ -> defaultGame


newGame : Model -> Model
newGame game =
  { defaultGame | seed <- game.seed }
  |> newLevel


newLevel : Model -> Model
newLevel game =
  let
    asteroidCount = Constants.startAsteroidCount + game.level * 2
    (asteroids', seed') =
      Random.generate
        (Random.list asteroidCount (Random.customGenerator Asteroid.newAsteroid))
        game.seed
  in
    { game
    | asteroids <- List.map Asteroid.tickAsteroid asteroids'
    , seed <- seed'
    , level <- game.level + 1
    }


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
  game
  |> moveGameItems
  |> shotCollisions
  |> checkForNewLevel


moveGameItems : Model -> Model
moveGameItems game =
  { game
  | ship <- Ship.moveShip game.ship game.arrows
  , shots <- List.filterMap Shot.tickShot game.shots
  , asteroids <- List.map Asteroid.tickAsteroid game.asteroids
  }


shotCollisions : Model -> Model
shotCollisions game =
  let
    shotCollision shot game' =
      let
        (shot', asteroids', seed') = shotVsAsteroids shot game'.asteroids game'.seed
      in
        case shot' of
          Just shot'' ->
            { game'
            | shots <- shot'' :: game'.shots
            }
          Nothing ->
            { game'
            | asteroids <- asteroids'
            , seed <- seed'
            }
  in
    List.foldr
      shotCollision
      { game | shots <- [] }
      game.shots


checkForNewLevel : Model -> Model
checkForNewLevel game =
  case game.asteroids of
    [] -> newLevel game
    _ -> game


shotVsAsteroids : Shot -> List Asteroid -> Seed -> (Maybe Shot, List Asteroid, Seed)
shotVsAsteroids shot asteroids seed =
  let
    shotVsAsteroids' (_, asteroidsToCheck, asteroidsResult, seed') =
      case asteroidsToCheck of
        asteroid :: tail ->
          if shotAsteroidCollisionTest shot asteroid then
            let
              destruction = destroyAsteroid asteroid seed'
              asteroids' = List.append asteroidsResult tail
            in
              case destruction of
                Just (a, b, seed'') ->
                  Trampoline.Done
                    (True, [], a :: b :: asteroids', seed'')
                Nothing ->
                  Trampoline.Done
                    (True, [], asteroids', seed')
          else
            Trampoline.Continue
              (\_ -> shotVsAsteroids' (False, tail, asteroid :: asteroidsResult, seed'))
        [] -> Trampoline.Done (False, [], asteroidsResult, seed')

    (collision, _, asteroids', seed') =
      Trampoline.trampoline (shotVsAsteroids' (False, asteroids, [], seed))
  in
    if collision then
      (Nothing, asteroids', seed')
    else
      (Just shot, asteroids', seed')


shotAsteroidCollisionTest : Shot -> Asteroid -> Bool
shotAsteroidCollisionTest shot asteroid =
  circlesOverlap
    (shot.position, Constants.shotSize) (asteroid.position, (asteroidSize asteroid))


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


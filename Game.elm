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
import Explosion exposing (..)
import KeyboardHelpers
import Random exposing(Seed)
import Ship exposing (..)
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
  , explosions : List Explosion
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
  , explosions = []
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
  |> shipCollisions
  |> tickExplosions
  |> checkForNewLevel
  |> tickShipState


moveGameItems : Model -> Model
moveGameItems game =
  { game
  | ship <- Ship.moveShip game.ship game.arrows
  , shots <- List.filterMap Shot.tickShot game.shots
  , asteroids <- List.map Asteroid.tickAsteroid game.asteroids
  }


checkForNewLevel : Model -> Model
checkForNewLevel game =
  case game.asteroids of
    [] -> newLevel game
    _ -> game


shotCollisions : Model -> Model
shotCollisions game =
  let
    shotCollision shot game' =
      let
        (shot', game'') = shotVsAsteroids shot game'
      in
        case shot' of
          Just shot'' ->
            { game''
            | shots <- shot'' :: game'.shots
            }
          Nothing -> game''
  in
    List.foldr
      shotCollision
      { game | shots <- [] }
      game.shots


shotVsAsteroids : Shot -> Model -> (Maybe Shot, Model)
shotVsAsteroids shot game =
  let
    shotVsAsteroids' (_, asteroidsToCheck, game') =
      case asteroidsToCheck of
        asteroid :: tail ->
          if shotVsAsteroid shot asteroid then
            let
              destruction = destroyAsteroid asteroid game'.seed
              asteroids' = List.append game'.asteroids tail
            in
              case destruction of
                Just (a, b, seed') ->
                  let game'' =
                    { game' | asteroids <- a :: b :: asteroids', seed <- seed' }
                      |> addExplosion asteroid.position
                  in
                    Trampoline.Done (True, [], game'')
                Nothing ->
                  let game'' =
                    { game' | asteroids <- asteroids' }
                      |> addExplosion asteroid.position
                  in
                    Trampoline.Done (True, [], game'')
          else
            Trampoline.Continue
              (\_ -> shotVsAsteroids'
                (False, tail, { game' | asteroids <- asteroid :: game'.asteroids }))
        [] -> Trampoline.Done (False, [], game')

    (collision, _, game') =
      Trampoline.trampoline
        (shotVsAsteroids' (False, game.asteroids, { game | asteroids <- [] }))
  in
    if collision then
      (Nothing, game')
    else
      (Just shot, game')


asteroidCollisionTest : (Vec2, Float) -> Asteroid -> Bool
asteroidCollisionTest test asteroid =
  circlesOverlap test (asteroid.position, (asteroidSize asteroid))


shotVsAsteroid : Shot -> Asteroid -> Bool
shotVsAsteroid shot asteroid =
  asteroidCollisionTest (shot.position, Constants.shotSize) asteroid


shipVsAsteroid : Ship -> Asteroid -> Bool
shipVsAsteroid ship asteroid =
  asteroidCollisionTest (ship.position, Constants.shipSizeForCollisions) asteroid


shipCollisions : Model -> Model
shipCollisions game =
  case game.ship.status of
    Alive ->
      let
        collision =
          List.any (\asteroid -> shipVsAsteroid game.ship asteroid) game.asteroids
      in
        if collision then
          { game
          | ship <- killShip game.ship
          } |> addExplosion game.ship.position
        else
          game
    _ -> game


addShot : Model -> Model
addShot game =
  case game.ship.status of
    Ship.Dead _ -> game
    _ ->
      let
        shotOffset = Constants.shipSize / 2 + Constants.shotSize / 2
        shotPosition = rotVec game.ship.angle { x = 0.0, y = shotOffset }
          |> addVec game.ship.position
      in
        { game
        | shots <- (newShot shotPosition game.ship.angle) :: game.shots
        }


addExplosion : Vec2 -> Model -> Model
addExplosion position game =
  let
    (explosion, seed) = newExplosion position game.seed
  in
    { game
    | explosions <- explosion :: game.explosions
    , seed <- seed
    }


tickExplosions : Model -> Model
tickExplosions game =
  { game
  | explosions <- List.filterMap tickExplosion game.explosions
  }


tickShipState : Model -> Model
tickShipState game =
  case game.ship.status of
    Ship.Dead _ ->
      let
        ship' = tickDeadShip game.ship
      in
        case ship' of
          Just ship ->
            { game | ship <- ship }
          Nothing ->
            { game | ship <- defaultShip }
    _ -> game

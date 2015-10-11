module Game
  ( Update(..)
  , Mode(..)
  , Model
  , intro
  , updateGame
  ) where

import Asteroid exposing (..)
import Constants
import Data.Vec2 exposing (..)
import Debug
import Explosion exposing (..)
import KeyboardHelpers
import Random exposing(Seed)
import RandomHelpers exposing (..)
import Ship exposing (..)
import Shot exposing (..)
import Saucer exposing (..)
import Trampoline
import Vec2Helpers exposing (..)


type Update
  = Arrows KeyboardHelpers.Arrows
  | Wasd KeyboardHelpers.Arrows
  | Tick Float
  | Space Bool
  | Escape Bool
  | Shift Bool
  | StartTime Float

type Mode
  = Intro
  | Play
  | Pause
  | GameOver


type alias Model =
  { mode : Mode
  , arrows : KeyboardHelpers.Arrows
  , ship : Maybe Ship
  , saucer : Maybe Saucer
  , shots : List Shot
  , asteroids : List Asteroid
  , explosions : List Explosion
  , score : Int
  , level : Int
  , lives : Int
  , tickCount : Int
  , nextSaucerTickCount : Int
  , seed : Seed
  }


defaultGame : Model
defaultGame =
  { mode = Play
  , arrows = KeyboardHelpers.defaultArrows
  , ship = Nothing
  , saucer = Nothing
  , shots = []
  , asteroids = []
  , explosions = []
  , score = 0
  , level = 0
  , lives = 0
  , tickCount = 0
  , nextSaucerTickCount = 0
  , seed = Random.initialSeed 0
  }


intro : Update -> Model
intro input =
  case input of
    StartTime time ->
      let
        seed = Random.initialSeed (round time)
        (asteroids, seed') =
          Random.generate
            (Random.list Constants.newGameAsteroidCount
              <| Random.customGenerator Asteroid.randomAsteroid)
            seed
        (saucer, seed'') = newSaucer 0 seed'
      in
        { defaultGame
        | mode <- Intro
        , asteroids <- asteroids
        , saucer <- Just saucer
        , seed <- seed''
        }
    _ -> defaultGame


newGame : Model -> Model
newGame game =
  { defaultGame
  | ship <- Just newShip
  , lives <- Constants.startLives
  }
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
    , tickCount <- 0
    , saucer <- Nothing
    } |> scheduleSaucer


updateGame : Update -> Model -> Model
updateGame input game =
  case input of
    Arrows arrows -> { game | arrows <- arrows }
    Wasd wasd -> { game | arrows <- wasd }
    Tick _ -> tickGame game
    Space down ->
      case game.mode of
        Play -> if down then addShot game else game
        _ -> changeGameMode game
    Escape down ->
      if down then
        case game.mode of
          Play -> changeGameMode game
          Pause -> changeGameMode game
          _ -> game
      else game
    Shift down ->
      if down then hyperspace game else game
    _ -> game


changeGameMode : Model -> Model
changeGameMode game =
  case game.mode of
    Intro -> newGame game
    Play -> { game | mode <- Pause }
    Pause -> { game | mode <- Play }
    GameOver -> newGame game


tickGame : Model -> Model
tickGame game =
  Debug.watch "Game" <|
  case game.mode of
    Intro -> tickPlay game
    Play -> tickPlay game
    GameOver -> tickPlay game
    _ -> game


tickPlay : Model -> Model
tickPlay game =
  { game | tickCount <- game.tickCount + 1 }
  |> moveGameItems
  |> shotCollisions
  |> shipVsSaucer
  |> shipVsAsteroids
  |> saucerVsAsteroids
  |> tickExplosions
  |> tickShipState
  |> tickSaucer
  |> checkForNewLevel
  |> checkForGameOver


moveGameItems : Model -> Model
moveGameItems game =
  { game
  | ship <- Ship.moveShip game.ship game.arrows
  , saucer <- Saucer.moveSaucer game.saucer
  , shots <- List.filterMap Shot.tickShot game.shots
  , asteroids <- List.map Asteroid.tickAsteroid game.asteroids
  }


checkForNewLevel : Model -> Model
checkForNewLevel game =
  case game.asteroids of
    [] -> newLevel game
    _ -> game


checkForGameOver : Model -> Model
checkForGameOver game =
  if game.mode /= Intro && game.ship == Nothing then
    { game | mode <- GameOver }
  else
    game


shotCollisions : Model -> Model
shotCollisions game =
  let
    shotCollision shot game' =
      let
        (shot', game'') =
          objectVsAsteroids shot game' shotVsAsteroid
          --|> shotVsShip
          |> shotVsSaucer
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


objectVsAsteroids : a -> Model -> (a -> Asteroid -> Bool) -> (Maybe a, Model)
objectVsAsteroids object game collisionTest =
  let
    objectVsAsteroids' (_, asteroidsToCheck, game') =
      case asteroidsToCheck of
        asteroid :: tail ->
          if collisionTest object asteroid then
            asteroidDestruction asteroid tail game'
          else
            let game'' = { game' | asteroids <- asteroid :: game'.asteroids }
            in Trampoline.Continue (\_ -> objectVsAsteroids' (False, tail, game''))
        [] -> Trampoline.Done (False, [], game')
    emptyAsteroids = { game | asteroids <- [] }
    (collision, _, game') =
      Trampoline.trampoline (objectVsAsteroids' (False, game.asteroids, emptyAsteroids))
  in
    if collision then
      (Nothing, game')
    else
      (Just object, game')


asteroidDestruction : Asteroid -> List Asteroid -> Model
  -> Trampoline.Trampoline (Bool, List Asteroid, Model)
asteroidDestruction asteroid tail game =
  let
    destruction = destroyAsteroid asteroid game.seed
    asteroids = List.append game.asteroids tail
    game' =
      case destruction of
        Just (a, b, seed') ->
          { game | asteroids <- a :: b :: asteroids, seed <- seed' }
            |> addExplosion asteroid.position
            |> addScore (asteroidScore asteroid)
        Nothing ->
          { game | asteroids <- asteroids }
            |> addExplosion asteroid.position
            |> addScore (asteroidScore asteroid)
  in
    Trampoline.Done (True, [], game')


shotVsSaucer : (Maybe Shot, Model) -> (Maybe Shot, Model)
shotVsSaucer (maybeShot, game) =
  case maybeShot of
    Nothing -> (maybeShot, game)
    Just shot ->
      case game.saucer of
        Nothing -> (maybeShot, game)
        Just saucer ->
          let collision =
            shotCollisionTest (saucer.position, saucerSizeForCollisions saucer) shot
          in
            if collision then
              ( Nothing
              , { game | saucer <- Nothing }
                |> addExplosion saucer.position
                |> addScore (saucerScore saucer)
                |> scheduleSaucer
              )
            else (maybeShot, game)


shotVsShip : (Maybe Shot, Model) -> (Maybe Shot, Model)
shotVsShip (maybeShot, game) =
  case maybeShot of
    Nothing -> (maybeShot, game)
    Just shot ->
      case game.ship of
        Nothing -> (maybeShot, game)
        Just ship ->
          if ship.status == Ship.Alive then
            let collision =
              shotCollisionTest (ship.position, Constants.shipSizeForCollisions) shot
            in
              if collision then
                ( Nothing
                , { game
                  | ship <- Just (killShip ship)
                  , lives <- game.lives - 1
                  }
                  |> addExplosion ship.position
                )
              else
                (maybeShot, game)
          else (maybeShot, game)


asteroidCollisionTest : (Vec2, Float) -> Asteroid -> Bool
asteroidCollisionTest test asteroid =
  circlesOverlap test (asteroid.position, (asteroidSize asteroid))


shotVsAsteroid : Shot -> Asteroid -> Bool
shotVsAsteroid shot asteroid =
  asteroidCollisionTest (shot.position, Constants.shotSize) asteroid


shotCollisionTest : (Vec2, Float) -> Shot -> Bool
shotCollisionTest test shot =
  circlesOverlap test (shot.position, Constants.shotSize)


shipVsAsteroid : Ship -> Asteroid -> Bool
shipVsAsteroid ship asteroid =
  asteroidCollisionTest (ship.position, Constants.shipSizeForCollisions) asteroid


saucerVsAsteroid : Saucer -> Asteroid -> Bool
saucerVsAsteroid saucer asteroid =
  asteroidCollisionTest (saucer.position, Constants.shipSizeForCollisions) asteroid


shipVsAsteroids : Model -> Model
shipVsAsteroids game =
  case game.ship of
    Nothing -> game
    Just ship ->
      case ship.status of
        Ship.Alive ->
          let
            (ship', game') = objectVsAsteroids ship game shipVsAsteroid
          in
            case ship' of
              Just _ -> game'
              Nothing ->
                { game'
                | ship <- Just (killShip ship)
                , lives <- game.lives - 1
                } |> addExplosion ship.position
        _ -> game


saucerVsAsteroids : Model -> Model
saucerVsAsteroids game =
  case game.saucer of
    Nothing -> game
    Just saucer ->
      let
        (saucer', game') = objectVsAsteroids saucer game saucerVsAsteroid
      in
        if saucer' == Nothing then
          { game' | saucer <- Nothing }
          |> addExplosion saucer.position
          |> scheduleSaucer
        else
          game


shipVsSaucer : Model -> Model
shipVsSaucer game =
  case game.ship of
    Nothing -> game
    Just ship ->
      if ship.status == Ship.Alive then
        case game.saucer of
          Nothing -> game
          Just saucer ->
            let
              collision = circlesOverlap
                (saucer.position, saucerSize saucer)
                (ship.position, Constants.shipSizeForCollisions)
            in
              if collision then
                { game
                | ship <- Just (killShip ship)
                , lives <- game.lives - 1
                , saucer <- Nothing
                }
                |> addExplosion ship.position
                |> addExplosion saucer.position
                |> addScore (saucerScore saucer)
                |> scheduleSaucer
              else game
      else game


addShot : Model -> Model
addShot game =
  case game.ship of
    Nothing -> game
    Just ship ->
      case ship.status of
        Ship.Dead -> game
        _ ->
          let
            shotOffset = Constants.shipSize / 2 + Constants.shotSize / 2
            shotPosition = rotVec ship.angle { x = 0.0, y = shotOffset }
              |> addVec ship.position
          in
            { game
            | shots <- (newShot shotPosition ship.angle) :: game.shots
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
  let
    maybeShip = Ship.tickShipState game.ship
    ship' =
      case maybeShip of
        Just _ -> maybeShip
        Nothing -> if game.lives == 0 then Nothing else (Just invincibleShip)
  in { game | ship <- ship' }


tickSaucer : Model -> Model
tickSaucer game =
  case game.saucer of
    Just saucer ->
      let
        (saucer', seed) = Saucer.tickSaucer saucer game.seed
        (shot, seed') = Saucer.maybeFireShot saucer' game.ship seed
        shots = case shot of
          Just shot' -> shot' :: game.shots
          Nothing -> game.shots
      in
        { game
        | saucer <- Just saucer'
        , seed <- seed'
        , shots <- shots
        }
    Nothing ->
      if game.tickCount == game.nextSaucerTickCount then
        let
          (saucer, seed) = newSaucer game.score game.seed
        in
          { game
          | saucer <- Just saucer
          , seed <- seed
          } |> scheduleSaucer
      else game


scheduleSaucer : Model -> Model
scheduleSaucer game =
  let
    (ticks, seed) = randomInt Constants.saucerTicksMin Constants.saucerTicksMax game.seed
  in
    { game
    | seed <- seed
    , nextSaucerTickCount <- game.tickCount + ticks
    }


addScore : Int -> Model -> Model
addScore score game =
  let
    scoreRank x = (floor ((toFloat x) / (toFloat Constants.extraLifeMultiple)))
    score' = game.score + score
    lives =
      if (scoreRank game.score) < (scoreRank score')
        then game.lives + 1
        else game.lives
  in
    { game
    | score <- score'
    , lives <- lives
    }


hyperspace : Model -> Model
hyperspace game =
  if game.mode == Play then
    let
      (ship, seed) = goIntoHyperspace game.ship game.seed
    in
      { game | ship <- ship, seed <- seed }
  else
    game

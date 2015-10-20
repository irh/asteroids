module Game
  ( Update(..)
  , Mode(..)
  , Model
  , intro
  , updateGame
  ) where

import Asteroid exposing (Asteroid)
import Constants
import Data.Vec2 exposing (Vec2)
import Debug
import Explosion exposing (Explosion)
import GameObject exposing (GameObject)
import KeyboardHelpers
import Random exposing(Seed)
import RandomHelpers
import Ship exposing (Ship)
import Shot exposing (Shot)
import Saucer exposing (Saucer)
import Trampoline


type Update
  = Arrows KeyboardHelpers.Arrows
  | Wasd KeyboardHelpers.Arrows
  | Tick Float
  | Space Bool
  | Escape Bool
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
  , nextLevelTickCount : Int
  , saucerCount : Int
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
  , nextLevelTickCount = 0
  , saucerCount = 0
  , seed = Random.initialSeed 0
  }


intro : Update -> Model
intro input =
  case input of
    StartTime time ->
      { defaultGame
      | seed <- Random.initialSeed (round time)
      } |> newIntro
    _ -> defaultGame


newIntro : Model -> Model
newIntro game =
  let
    (asteroids, seed) =
      Random.generate
        (Random.list Constants.introAsteroidCount
          <| Random.customGenerator Asteroid.randomAsteroid)
        game.seed
    (saucer, seed') = Saucer.newSaucer 0 (Constants.initialBigSaucerCount + 1) seed
  in
    { defaultGame
    | mode <- Intro
    , asteroids <- asteroids
    , saucer <- Just saucer
    , seed <- seed'
    }


newGame : Model -> Model
newGame game =
  { defaultGame
  | ship <- Just Ship.newShip
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
    , nextLevelTickCount <- 0
    , saucer <- Nothing
    } |> scheduleSaucer


updateGame : Update -> Model -> Model
updateGame input game =
  case input of
    Arrows arrows -> updateArrows game arrows
    Wasd wasd -> updateArrows game wasd
    Tick _ -> tickGame game
    Space down ->
      case game.mode of
        Play -> if down then fireShot game else game
        _ -> changeGameMode game
    Escape down ->
      if down then
        case game.mode of
          Play -> changeGameMode game
          Pause -> quitGame game
          _ -> game
      else game
    _ -> game


updateArrows : Model -> { x : Int, y : Int } -> Model
updateArrows game arrows =
  if arrows.y < 0 then
    hyperspace game
  else
    { game | arrows <- arrows }


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
  |> checkForNewLevel
  |> moveGameItems
  |> checkForOutOfBoundsSaucer
  |> shotCollisions
  |> shipVsSaucer
  |> shipVsAsteroids
  |> saucerVsAsteroids
  |> tickExplosions
  |> tickShipState
  |> tickSaucer
  |> scheduleNewLevel
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
  if game.tickCount == game.nextLevelTickCount then
    case game.mode of
      Intro -> newIntro game
      _ -> newLevel game
  else game


scheduleNewLevel : Model -> Model
scheduleNewLevel game =
  if game.asteroids == []
     && game.saucer == Nothing
     && game.nextLevelTickCount < game.tickCount
  then
    { game
    | nextLevelTickCount <- game.tickCount + Constants.nextLevelDelayTicks
    , nextSaucerTickCount <- 0
    }
  else game


quitGame : Model -> Model
quitGame game =
  let ship' =
    case game.ship of
      Just ship -> Just (Ship.killShip ship)
      Nothing -> Nothing
  in
    { game
    | mode <- GameOver
    , ship <- ship'
    , lives <- 0
    }


checkForGameOver : Model -> Model
checkForGameOver game =
  if game.mode /= Intro && game.ship == Nothing then
    { game | mode <- GameOver }
  else game


checkForOutOfBoundsSaucer : Model -> Model
checkForOutOfBoundsSaucer game =
  case game.saucer of
    Nothing -> game
    Just saucer ->
      let outOfBounds =
        saucer.position.x < Constants.gameBoundsMinX
        || saucer.position.x > Constants.gameBoundsMaxX
      in
        if outOfBounds then
          { game | saucer <- Nothing } |> scheduleSaucer
        else game


shotCollisions : Model -> Model
shotCollisions game =
  let
    shotCollision shot game' =
      let
        (shot', game'') =
          objectVsAsteroids shot game'
          |> objectVsObject game'.ship doShipCollision
          |> objectVsObject game'.saucer (doSaucerCollision True)
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


objectVsAsteroids : GameObject a -> Model -> (Maybe (GameObject a), Model)
objectVsAsteroids object game =
  let
    objectVsAsteroids' (_, asteroidsToCheck, game') =
      case asteroidsToCheck of
        asteroid :: tail ->
          if GameObject.collisionTest object asteroid then
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
    destruction = Asteroid.destroyAsteroid asteroid game.seed
    asteroids = List.append game.asteroids tail
    score = Asteroid.asteroidScore asteroid
    game' =
      case destruction of
        Just (a, b, seed') ->
          { game | asteroids <- a :: b :: asteroids, seed <- seed' }
        Nothing ->
          { game | asteroids <- asteroids }
  in
    Trampoline.Done
      (True, [],
        game'
        |> addExplosion asteroid.position
        |> addScore score
      )


objectVsObject :
  Maybe (GameObject b)
  -> (GameObject a -> GameObject b -> Model -> Model)
  -> (Maybe (GameObject a), Model)
  -> (Maybe (GameObject a), Model)
objectVsObject maybeObjectB collisionFunction (maybeObject, game) =
  case maybeObject of
    Nothing -> (maybeObject, game)
    Just object ->
      case maybeObjectB of
        Nothing -> (maybeObject, game)
        Just objectB ->
          if GameObject.collisionTest object objectB then
            (Nothing, collisionFunction object objectB game)
          else
            (maybeObject, game)


doShipCollision : GameObject a -> Ship -> Model -> Model
doShipCollision _ ship game =
  if ship.status == Ship.Alive then
    { game
    | ship <- Just (Ship.killShip ship)
    , lives <- game.lives - 1
    }
    |> addExplosion ship.position
  else game


doSaucerCollision : Bool -> GameObject a -> Saucer -> Model -> Model
doSaucerCollision score _ saucer game =
  { game | saucer <- Nothing }
  |> addExplosion saucer.position
  |> addScore (if score then (Saucer.saucerScore saucer) else 0)
  |> scheduleSaucer


doShipSaucerCollision : Ship -> Saucer -> Model -> Model
doShipSaucerCollision ship saucer game =
  if ship.status == Ship.Alive then
    doShipCollision saucer ship game
    |> doSaucerCollision True ship saucer
  else game


shipVsSaucer : Model -> Model
shipVsSaucer game =
  let
    (_, game') =
      (game.ship, game)
      |> objectVsObject game.saucer doShipSaucerCollision
  in
    game'


gameObjectVsAsteroids :
  Maybe (GameObject a)
  -> (GameObject a -> GameObject a -> Model -> Model)
  -> Model
  -> Model
gameObjectVsAsteroids maybeObject collisionFunction game =
  case maybeObject of
    Nothing -> game
    Just object ->
      let (object', game') = objectVsAsteroids object game
      in case object' of
        Just _ -> game'
        Nothing -> collisionFunction object object game'


shipVsAsteroids : Model -> Model
shipVsAsteroids game =
  case game.ship of
    Nothing -> game
    Just ship ->
      if ship.status == Ship.Alive then
        gameObjectVsAsteroids game.ship doShipCollision game
      else game


saucerVsAsteroids : Model -> Model
saucerVsAsteroids game =
  gameObjectVsAsteroids game.saucer (doSaucerCollision False) game


fireShot : Model -> Model
fireShot game =
  case Ship.fireShot game.ship of
    Nothing -> game
    Just shot ->
      { game
      | shots <- shot :: game.shots
      }


addExplosion : Vec2 -> Model -> Model
addExplosion position game =
  let
    (explosion, seed) = Explosion.newExplosion position game.seed
  in
    { game
    | explosions <- explosion :: game.explosions
    , seed <- seed
    }


tickExplosions : Model -> Model
tickExplosions game =
  { game
  | explosions <- List.filterMap Explosion.tickExplosion game.explosions
  }


tickShipState : Model -> Model
tickShipState game =
  let
    maybeShip = Ship.tickShipState game.ship
    ship' =
      case maybeShip of
        Just _ -> maybeShip
        Nothing -> if game.lives == 0 then Nothing else (Just Ship.invincibleShip)
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
          (saucer, seed) = Saucer.newSaucer game.score game.saucerCount game.seed
        in
          { game
          | saucer <- Just saucer
          , saucerCount <- game.saucerCount + 1
          , seed <- seed
          } |> scheduleSaucer
      else game


scheduleSaucer : Model -> Model
scheduleSaucer game =
  let
    (ticks, seed) =
      RandomHelpers.randomInt Constants.saucerTicksMin Constants.saucerTicksMax game.seed
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
      (ship, seed) = Ship.goIntoHyperspace game.ship game.seed
    in
      { game | ship <- ship, seed <- seed }
  else game

module View (view) where

import Asteroid exposing (Asteroid, Kind)
import Color exposing (Color, rgb)
import Constants
import Data.Vec2 exposing (..)
import Explosion exposing (Explosion)
import Game
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Point exposing (asTuple)
import Ship exposing (Ship)
import Shot exposing (Shot)
import Text
import Window


backgroundColor = rgb 0 0 0
borderColor = rgb 255 255 255
shipColor = rgb 255 255 255
asteroidColor = rgb 255 255 255


view : (Int, Int) -> Game.Model -> Element
view (w, h) game =
  let
    (w', h') = (toFloat w, toFloat h)
    factor = min w' h' - 2.0
    (width, height) = ((Constants.gameWidth * factor, Constants.gameHeight * factor))
    background =
      rect width height
      |> filled backgroundColor
    border =
      rect width height
      |> outlined borderLineStyle
      |> move (0, 0)
    ship = renderShip game.ship factor
    shots = List.concatMap (\shot -> renderShot shot factor) game.shots
    asteroids =
      List.map (\asteroid -> renderAsteroid asteroid factor) game.asteroids
    explosions =
      List.map (\explosion -> renderExplosion explosion factor) game.explosions
  in
    collage (floor width) (floor height)
      (List.concat [[background], ship, shots, asteroids, explosions, [border]])
    |> container w h middle
    |> color backgroundColor


scaleTuple : (Float, Float) -> Float -> (Float, Float)
scaleTuple (x, y) factor =
  (x * factor, y * factor)


shipWidth = 0.6 / 2
shipHeight = 1.0 / 2
crossBarX = shipWidth * 2.0 / 3.0
crossBarY = -shipHeight + shipHeight / 3.0
flameX = crossBarX * 0.75
flameOffset = shipHeight / 4.0
flameY = crossBarY - flameOffset
flameY2 = -shipHeight - flameOffset
shotRadius = shipHeight * Constants.shotShipRatio


renderShip : Ship -> Float -> List Form
renderShip ship factor =
  case ship.status of
    Ship.Alive -> renderLiveShip ship factor
    Ship.Dead _ -> renderShipDebris ship factor


renderLiveShip : Ship -> Float -> List Form
renderLiveShip ship factor =
  let
    shipPosition = (scaleTuple (asTuple ship.position) factor)
    shipSize = (Constants.shipSize * factor)
    shipTransform = (\path ->
      scalePath path shipSize
      |> traced shipLineStyle
      |> move shipPosition
      |> rotate ship.angle
      )
  in
    if ship.thrust then
      [ shipTransform shipPath, shipTransform thrustPath ]
    else
      [ shipTransform shipPath ]


renderShipDebris : Ship -> Float -> List Form
renderShipDebris ship factor =
  case ship.status of
    Ship.Dead tickCount ->
      let
        progress = (toFloat tickCount) / (toFloat Constants.deadShipTime) + 0.5
        shipSize = Constants.shipSize * factor
        debrisSize = progress * Constants.shipDebrisSize * factor
        renderDebris (lineSize, angle, moveAngle, yOffset) =
          segment
            (scaleTuple (0, -0.5) (lineSize * shipSize))
            (scaleTuple (0, 0.5) (lineSize * shipSize))
          |> traced shipLineStyle
          |> rotate angle
          |> move (0, yOffset * shipSize)
          |> move (asTuple (rotVec moveAngle { x = 0, y = debrisSize }))
        debris = List.map renderDebris
          [ (1.0, pi / 8, -pi / 6, -0.25)
          , (1.0, -pi / 8, pi / 6, -0.25)
          , (0.5, pi / 2, pi, 0)
          ]
      in
        [ group debris
        |> move (scaleTuple (asTuple ship.position) factor)
        |> rotate ship.angle
        ]
    _ -> []


renderShot : Shot -> Float -> List Form
renderShot shot factor =
  let
    shotTransform = (\path ->
      scalePath path (Constants.shipSize * factor)
      |> traced shipLineStyle
      |> move (scaleTuple (asTuple shot.position) factor)
      )
  in
    [ shotTransform (segment (-shotRadius, 0) (shotRadius, 0))
    , shotTransform (segment (0, -shotRadius) (0, shotRadius))
    ]


renderAsteroid : Asteroid -> Float -> Form
renderAsteroid asteroid factor =
  let
    size = factor * case asteroid.size of
      Asteroid.Big -> Constants.asteroidSizeBig
      Asteroid.Medium -> Constants.asteroidSizeMedium
      Asteroid.Small -> Constants.asteroidSizeSmall
  in
    scalePath (asteroidPath asteroid) size
    |> outlined asteroidLineStyle
    |> move (scaleTuple (asTuple asteroid.position) factor)
    |> rotate asteroid.angle


renderExplosion : Explosion -> Float -> Form
renderExplosion explosion factor =
  let
    progress = (toFloat explosion.tickCount) / (toFloat Constants.explosionLifetime)
    size = factor * progress * Constants.explosionSize
    debrisSize = factor * Constants.debrisSize
    renderDebris position =
      square debrisSize
      |> outlined asteroidLineStyle
      |> move (scaleTuple position (debrisSize * size))
    debris = List.map renderDebris
      [ (0.8, 0.3)
      , (0.1, 0.3)
      , (0.5, 0.6)
      , (0.5, -0.2)
      , (0.2, -0.4)
      , (0.6, -0.9)
      , (-1.0, -0.3)
      , (-0.8, -0.3)
      , (-0.2, -0.6)
      , (-0.5, 0.6)
      , (-0.9, 0.7)
      , (-0.4, 0.3)
      ]
  in
    group debris
    |> move (scaleTuple (asTuple explosion.position) factor)
    |> rotate explosion.angle


scalePath : Path -> Float -> Path
scalePath path factor =
  List.map (\(x, y) -> (x * factor, y * factor)) path


shipPath : Path
shipPath =
  [ (-shipWidth, -shipHeight)
  , (0, shipHeight)
  , (shipWidth, -shipHeight)
  , (crossBarX, crossBarY)
  , (-crossBarX, crossBarY)
  , (-shipWidth, -shipHeight)
  ]


thrustPath : Path
thrustPath =
  [ (-flameX, flameY)
  , (flameX, flameY)
  , (0, flameY2)
  , (-flameX, flameY)
  ]


shotPath : Path
shotPath =
  [ (-shotRadius, 0)
  , (0, shotRadius)
  , (shotRadius, 0)
  , (0, -shotRadius)
  , (-shotRadius, 0)
  ]


asteroidPath : Asteroid -> Path
asteroidPath asteroid  =
  case asteroid.kind of
    Asteroid.A -> asteroidPathA
    Asteroid.B -> asteroidPathB
    Asteroid.C -> asteroidPathC


asteroidPathA : Path
asteroidPathA =
  [ (-1.0, 0.5)
  , (-0.5, 1.0)
  , (0.0, 0.7)
  , (0.5, 1.0)
  , (1.0, 0.5)
  , (0.8, 0.0)
  , (1.0, -0.5)
  , (0.30, -1.0)
  , (-0.5, -1.0)
  , (-1.0, -0.6)
  ]


asteroidPathB : Path
asteroidPathB =
  [ (-1.0, 0.5)
  , (-0.2, 0.5)
  , (-0.5, 1.0)
  , (0.2, 1.0)
  , (1.0, 0.5)
  , (1.0, 0.3)
  , (0.4, 0.0)
  , (1.0, -0.5)
  , (0.4, -1.0)
  , (0.2, -0.7)
  , (-0.5, -1.0)
  , (-1.0, -0.3)
  ]


asteroidPathC : Path
asteroidPathC =
  [ (-1.0, 0.8)
  , (-0.4, 1.0)
  , (0.2, 0.8)
  , (0.6, 1.0)
  , (0.7, 1.0)
  , (0.9, 0.8)
  , (1.0, 0.1)
  , (0.9, -0.25)
  , (1.0, -0.4)
  , (0.9, -0.9)
  , (0.5, -1.0)
  , (0.2, -1.0)
  , (0.0, -0.8)
  , (-0.333, -1.0)
  , (-0.7, -1.0)
  , (-0.95, -0.4)
  , (-1.0, -0.333)
  , (-0.8, 0.1)
  , (-1.0, 0.333)
  ]


shipLineStyle : LineStyle
shipLineStyle =
  { defaultLine
  | color <- shipColor
  , join <- Clipped
  }


asteroidLineStyle : LineStyle
asteroidLineStyle =
  { defaultLine
  | color <- asteroidColor
  }


borderLineStyle : LineStyle
borderLineStyle =
  { defaultLine
  | color <- borderColor
  , width <- 2
  }


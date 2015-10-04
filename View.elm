module View (view) where

import Color exposing (Color, rgb)
import Constants
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
  in
    collage (floor width) (floor height)
      (List.concat [[background], ship, shots, [border]])
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
  let
    shipSize = (Constants.shipSize * factor)
    shipPosition = (scaleTuple (asTuple ship.position) factor)
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


shipLineStyle : LineStyle
shipLineStyle =
  { defaultLine
  | color <- shipColor
  , join <- Clipped
  }


borderLineStyle : LineStyle
borderLineStyle =
  { defaultLine
  | color <- borderColor
  , width <- 2
  }


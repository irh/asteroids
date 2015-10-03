module View (view) where

import Color exposing (Color, rgb)
import Constants
import Game
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Point exposing (asTuple)
import Ship exposing (Ship)
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
  in
    collage (floor width) (floor height)
      (List.concat [[background], ship, [border]])
    |> container w h middle
    |> color backgroundColor


scaleTuple : (Float, Float) -> Float -> (Float, Float)
scaleTuple (x, y) factor =
  (x * factor, y * factor)


renderShip : Ship -> Float -> List Form
renderShip ship factor =
  let
    shipSize = (Ship.shipSize * factor)
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


scalePath : Path -> Float -> Path
scalePath path factor =
  List.map (\(x, y) -> (x * factor, y * factor)) path


shipWidth = 0.6 / 2
shipHeight = 1.0 / 2
crossBarX = shipWidth * 2.0 / 3.0
crossBarY = -shipHeight + shipHeight / 3.0
flameX = crossBarX * 0.75
flameOffset = shipHeight / 4.0
flameY = crossBarY - flameOffset
flameY2 = -shipHeight - flameOffset


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


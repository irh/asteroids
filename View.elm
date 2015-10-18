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
import Saucer exposing (Saucer)
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
    background = rect width height |> filled backgroundColor
    border = rect width height |> outlined borderLineStyle |> move (0, 0)
    score = case game.mode of
      Game.Intro -> group []
      _ -> scoreText game.score width height factor
  in
    collage (floor width) (floor height)
      [ background
      , renderShip game.ship factor
      , renderLives game.lives factor width height
      , renderSaucer game.saucer factor
      , renderObjects game.shots renderShot factor
      , renderObjects game.asteroids renderAsteroid factor
      , renderObjects game.explosions renderExplosion factor
      , border
      , score
      , renderGameText game.mode factor
      ]
    |> container w h middle
    |> color backgroundColor


scaleTuple : (Float, Float) -> Float -> (Float, Float)
scaleTuple (x, y) factor = (x * factor, y * factor)


shipWidth = 0.6 / 2
shipHeight = 1.0 / 2
crossBarX = shipWidth * 2.0 / 3.0
crossBarY = -shipHeight + shipHeight / 3.0
flameX = crossBarX * 0.75
flameOffset = shipHeight / 4.0
flameY = crossBarY - flameOffset
flameY2 = -shipHeight - flameOffset
shotRadius = shipHeight * Constants.shotShipRatio


renderObjects : List a -> (a -> Float -> Form) -> Float -> Form
renderObjects list renderFunction factor =
  group (List.map(\object -> renderFunction object factor) list)


renderShip : Maybe Ship -> Float -> Form
renderShip maybeShip factor =
  case maybeShip of
    Just ship ->
      case ship.status of
        Ship.Dead -> renderShipDebris ship factor
        _ ->
          let showShip =
            case ship.status of
              Ship.Alive -> True
              Ship.Invincible -> ship.tickCount % 20 < 10
              _ -> False
          in if showShip then
            renderLiveShip ship factor
          else
            group []
    Nothing -> group []


renderLiveShip : Ship -> Float -> Form
renderLiveShip ship factor =
  let
    shipPosition = (scaleTuple (asTuple ship.position) factor)
    shipSize = (Constants.shipSize * factor)
    lineStyle = shipLineStyle
    shipTransform = (\path ->
      scalePath path shipSize
      |> traced lineStyle
      )
    showThrust = ship.thrust && (ship.tickCount % 3 == 0)
    shipPaths = if showThrust then [ shipPath, thrustPath ] else [ shipPath ]
    shipShape = List.map shipTransform shipPaths
  in
    group shipShape
    |> move shipPosition
    |> rotate ship.angle


renderShipDebris : Ship -> Float -> Form
renderShipDebris ship factor =
  let
    progress = (toFloat ship.tickCount) / (toFloat Constants.deadShipTime) + 0.5
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
    group debris
    |> move (scaleTuple (asTuple ship.position) factor)
    |> rotate ship.angle


renderLives : Int -> Float -> Float -> Float -> Form
renderLives lives factor width height =
  let
    shipSize = (Constants.lifeSize * factor)
    ship = scalePath shipPath shipSize
      |> traced shipLineStyle
    ships = List.repeat lives ship
      |> List.indexedMap (\i ship' -> (ship |> move ((toFloat i) * shipSize, 0)))
  in
    group ships
    |> move (-width / 2 + shipSize, height / 2 - shipSize)


renderSaucer : Maybe Saucer -> Float -> Form
renderSaucer maybeSaucer factor =
  case maybeSaucer of
    Nothing -> group []
    Just saucer ->
      let
        saucerSize = (Saucer.saucerSizeForView saucer) * factor
        saucerTransform = (\path ->
          scalePath path saucerSize
          |> outlined shipLineStyle
          )
        outlinePath =
          [ (-0.5, 0)
          , (-0.1667, 0.23)
          , (-0.08, 0.4)
          , (0.08, 0.4)
          , (0.1667, 0.23)
          , (0.5, 0)
          , (0.2, -0.2)
          , (-0.2, -0.2)
          ]
        saucerShapes = List.map saucerTransform
          [ outlinePath
          , [(-0.5, 0), (0.5, 0)]
          , [(-0.1667, 0.23), (0.1667, 0.23)]
          ]
      in
        group saucerShapes
        |> move (scaleTuple (asTuple saucer.position) factor)


renderShot : Shot -> Float -> Form
renderShot shot factor =
  let
    renderShotLine = (\path ->
      scalePath path (Constants.shipSize * factor)
      |> traced shipLineStyle
      )
    shotLines =
      [ (segment (-shotRadius, 0) (shotRadius, 0))
      , (segment (0, -shotRadius) (0, shotRadius))
      ]
  in
    group (List.map renderShotLine shotLines)
    |> move (scaleTuple (asTuple shot.position) factor)


renderAsteroid : Asteroid -> Float -> Form
renderAsteroid asteroid factor =
  let
    size = factor * asteroid.size
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


backgroundLineStyle : LineStyle
backgroundLineStyle =
  { defaultLine
  | color <- backgroundColor
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


textColor = rgb 250 250 250


styledText : Text.Style -> String -> Form
styledText style string =
  Text.fromString string
  |> Text.style style
  |> Graphics.Collage.outlinedText shipLineStyle


textStyle : Color -> Float -> Text.Style
textStyle color height =
  { typeface = [ "monospace" ]
  , height = Just height
  , color = color
  , bold = False
  , italic = False
  , line = Nothing
  }


textSize : String -> Text.Style -> (Float, Float)
textSize string style =
  let
    text = Text.fromString string |> Text.style style |> centered
  in
    (toFloat (widthOf text), toFloat (heightOf text))


scoreText : Int -> Float -> Float -> Float -> Form
scoreText score width height factor =
  let
    scoreString = toString score
    style = textStyle textColor (Constants.scoreTextHeight * factor)
    (textWidth, textHeight) = textSize scoreString style
    xOffset = width / 2 - textWidth / 2 - Constants.scoreTextHeight * factor / 3
    yOffset = height / 2 - textHeight / 2 + Constants.scoreTextHeight * factor / 10
  in
    styledText style scoreString
    |> move (xOffset, yOffset)


renderGameText : Game.Mode -> Float -> Form
renderGameText mode factor =
  if mode == Game.Play then group []
  else
    let
      style = textStyle textColor (Constants.gameTextHeight * factor)
      (first, second) = case mode of
        Game.Intro -> ("ASTEROIDS", "PRESS SPACE TO PLAY")
        Game.Pause -> ("PAUSED", "PRESS SPACE TO CONTINUE")
        Game.GameOver -> ("GAME OVER", "PRESS SPACE TO PLAY AGAIN")
        _ -> ("", "")
      (_, textHeight) = textSize "_" style
      yOffset = textHeight / 2 + Constants.gameTextHeight * factor / 5
      line1 = styledText style first
      line2 = styledText style second
    in
      group [ line1 |> move (0, yOffset)
            , line2 |> move (0, -yOffset)
            ]


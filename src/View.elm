module View (view) where

import Asteroid exposing (Asteroid, Kind)
import Color exposing (Color, rgb)
import Constants
import Vec2 exposing (Vec2)
import Explosion exposing (Explosion)
import Game
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Html exposing (Html)
import Point exposing (asTuple)
import Ship exposing (Ship)
import Shot exposing (Shot)
import Saucer exposing (Saucer)
import Text


backgroundColor = rgb 0 0 0
borderColor = rgb 255 255 255
shipColor = rgb 255 255 255
asteroidColor = rgb 255 255 255


view : Signal.Address Game.Action -> Game.Model -> Html
view _ game =
  let
    (w, h) = (fst(game.window), snd(game.window))
    factor = min (toFloat w) (toFloat h) - 2.0
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
    |> Html.fromElement


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
shotRadius = 0.15


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
    shipTransform = (\points ->
      Graphics.Collage.polygon (scalePoints points shipSize)
      |> outlined lineStyle
      )
    showThrust = ship.thrust && (ship.tickCount % 3 == 0)
    points = if showThrust then [ shipPoints, thrustPoints ] else [ shipPoints ]
    shipShape = List.map shipTransform points
  in
    group shipShape
    |> move shipPosition
    |> rotate ship.angle


renderShipDebris : Ship -> Float -> Form
renderShipDebris ship factor =
  let
    progress = (toFloat ship.tickCount) / (toFloat Constants.deadShipTicks) + 0.5
    shipSize = Constants.shipSize * factor
    debrisSize = progress * Constants.shipDebrisSize * factor
    renderDebris (lineSize, angle, moveAngle, xOffset, yOffset) =
      segment
        (scaleTuple (0, -0.5) (lineSize * shipSize))
        (scaleTuple (0, 0.5) (lineSize * shipSize))
      |> traced shipLineStyle
      |> rotate angle
      |> move (xOffset * shipSize, yOffset * shipSize)
      |> move (asTuple (Vec2.rotate moveAngle { x = 0, y = debrisSize }))
    debris = List.map renderDebris
      [ (0.45, pi / 8, -pi / 6, -0.05, 0.15)
      , (0.5, pi / 12, -pi / 3, -0.05, -0.35)
      , (0.5, -pi / 8, pi / 6, 0.05, 0.15)
      , (0.45, -pi / 12, pi / 3, 0.05, -0.35)
      , (0.4, pi / 2, pi, 0, 0.0)
      ]
  in
    group debris
    |> move (scaleTuple (asTuple ship.position) factor)
    |> rotate ship.angle


renderLives : Int -> Float -> Float -> Float -> Form
renderLives lives factor width height =
  let
    shipSize = (Constants.lifeSize * factor)
    ship = Graphics.Collage.path (scalePoints shipPoints shipSize)
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
          Graphics.Collage.polygon (scalePoints path saucerSize)
          |> outlined shipLineStyle
          )
        outlinePoints =
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
          [ outlinePoints
          , [(-0.5, 0), (0.5, 0)]
          , [(-0.1667, 0.23), (0.1667, 0.23)]
          ]
      in
        group saucerShapes
        |> move (scaleTuple (asTuple saucer.position) factor)


renderShot : Shot -> Float -> Form
renderShot shot factor =
  square (shotRadius * Constants.shipSize * factor)
  |> filled shipColor
  |> move (scaleTuple (asTuple shot.position) factor)


renderAsteroid : Asteroid -> Float -> Form
renderAsteroid asteroid factor =
  let
    size = factor * asteroid.size
  in
    Graphics.Collage.polygon (scalePoints (asteroidPoints asteroid) size)
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


type alias Points = List (Float, Float)

scalePoints : Points -> Float -> Points
scalePoints path factor =
  List.map (\(x, y) -> (x * factor, y * factor)) path


shipPoints : Points
shipPoints =
  [ (-shipWidth, -shipHeight)
  , (0, shipHeight)
  , (shipWidth, -shipHeight)
  , (crossBarX, crossBarY)
  , (-crossBarX, crossBarY)
  ]


thrustPoints : Points
thrustPoints =
  [ (-flameX, flameY)
  , (flameX, flameY)
  , (0, flameY2)
  ]


asteroidPoints : Asteroid -> Points
asteroidPoints asteroid  =
  case asteroid.kind of
    Asteroid.A -> asteroidPointsA
    Asteroid.B -> asteroidPointsB
    Asteroid.C ->
      case asteroid.sizeClass of
        Asteroid.Small -> asteroidPointsCSmall
        _ -> asteroidPointsC


asteroidPointsA : Points
asteroidPointsA =
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


asteroidPointsB : Points
asteroidPointsB =
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


asteroidPointsC : Points
asteroidPointsC =
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

asteroidPointsCSmall : Points
asteroidPointsCSmall =
  [ (-1.0, 0.8)
  , (-0.4, 1.0)
  , (0.9, 0.8)
  , (1.0, 0.1)
  , (1.0, -0.4)
  , (0.9, -0.9)
  , (0.2, -1.0)
  , (0.0, -0.7)
  , (-0.333, -1.0)
  , (-0.7, -1.0)
  , (-1.0, -0.333)
  , (-0.8, 0.333)
  ]


shipLineStyle : LineStyle
shipLineStyle =
  { defaultLine
  | color = shipColor
  , join = Clipped
  }


backgroundLineStyle : LineStyle
backgroundLineStyle =
  { defaultLine
  | color = backgroundColor
  }


asteroidLineStyle : LineStyle
asteroidLineStyle =
  { defaultLine
  | color = asteroidColor
  }


borderLineStyle : LineStyle
borderLineStyle =
  { defaultLine
  | color = borderColor
  , width = 2
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


textHeight : Text.Style -> Float
textHeight style =
  let (_, height) = textSize "_" style
  in height


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


introText =
  [ "---------"
  , "ASTEROIDS"
  , "---------"
  , ""
  , "UP - THRUST"
  , "DOWN - HYPERSPACE"
  , "LEFT/RIGHT - TURN"
  , "SPACE - FIRE"
  , "ESC - PAUSE"
  , "M - MUTE"
  , ""
  , "PRESS SPACE TO PLAY"
  ]

pauseText =
  [ "PAUSED"
  , ""
  , "PRESS SPACE TO CONTINUE"
  , "PRESS ESC TO QUIT"
  ]

gameOverText =
  [ "GAME OVER"
  , ""
  , "PRESS SPACE TO PLAY AGAIN"
  ]


renderGameText : Game.Mode -> Float -> Form
renderGameText mode factor =
  if mode == Game.Play then group []
  else
    let
      style = textStyle textColor (Constants.gameTextHeight * factor)
      lines = case mode of
        Game.Intro -> introText
        Game.Pause -> pauseText
        Game.GameOver -> gameOverText
        _ -> []
      lineCount = List.length lines
      centerLine = (toFloat lineCount - 1) / 2.0
      yOffset = (textHeight style) + Constants.gameTextHeight * factor / 5
      renderLine = (\index text ->
        let
          line = styledText style text
          lineOffset = ((toFloat index) - centerLine) * -yOffset
        in
          line |> move (0, lineOffset)
        )
    in
      group <| List.indexedMap renderLine lines

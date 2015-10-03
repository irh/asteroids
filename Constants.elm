module Constants where

import Data.Vec2 exposing (..)

gameWidth : Float
gameWidth = 1.0

gameHeight : Float
gameHeight = 2.0 / 3.0

gameBounds : (Vec2, Vec2)
gameBounds = ({x = -gameWidth / 2, y = -gameHeight / 2}, {x = gameWidth / 2, y = gameHeight / 2})

tickFps : Int
tickFps = 60



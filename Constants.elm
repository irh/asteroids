module Constants where

import Data.Vec2 exposing (Vec2)

gameWidth : Float
gameWidth = 1.0

gameHeight : Float
gameHeight = 2.0 / 3.0

gameBounds : (Vec2, Vec2)
gameBounds = ({x = -gameWidth / 2, y = -gameHeight / 2}, {x = gameWidth / 2, y = gameHeight / 2})

tickFps : Int
tickFps = 60

startAsteroidCount : Int
startAsteroidCount = 5

shipSize : Float
shipSize = 0.03

turningSpeed : Float
turningSpeed = 0.1

thrust : Vec2
thrust = { x = 0, y = 0.00015 }

spaceFriction : Float
spaceFriction = 0.988

maxMomentum : Float
maxMomentum = 0.0125

shotShipRatio : Float
shotShipRatio = 0.2

shotSize : Float
shotSize = shipSize * shotShipRatio

shotSpeed : Vec2
shotSpeed = { x = 0, y = 0.015 }

shotLifetime : Int
shotLifetime = 35

asteroidSpeedBig : Float
asteroidSpeedBig = 0.001

asteroidSpeedMedium : Float
asteroidSpeedMedium = 0.002

asteroidSpeedSmall : Float
asteroidSpeedSmall = 0.003

asteroidSizeBig : Float
asteroidSizeBig = 0.06

asteroidSizeMedium : Float
asteroidSizeMedium = 0.04

asteroidSizeSmall : Float
asteroidSizeSmall = 0.02

module Constants where

import Data.Vec2 exposing (Vec2)

gameWidth : Float
gameWidth = 1.0

gameHeight : Float
gameHeight = 3.0 / 4.0

gameBoundsMinX = -gameWidth / 2
gameBoundsMaxX = gameWidth / 2

gameBoundsMinY = -gameHeight / 2
gameBoundsMaxY = gameHeight / 2

gameBoundsMin : Vec2
gameBoundsMin = { x = gameBoundsMinX, y = gameBoundsMinY }

gameBoundsMax : Vec2
gameBoundsMax = { x = gameBoundsMaxX, y = gameBoundsMaxY }

gameBounds : (Vec2, Vec2)
gameBounds = (gameBoundsMin, gameBoundsMax)

tickFps : Int
tickFps = 60

startLives : Int
startLives = 3

extraLifeMultiple : Int
extraLifeMultiple = 10000

startAsteroidCount : Int
startAsteroidCount = 8

introAsteroidCount : Int
introAsteroidCount = 30

nextLevelDelayTicks : Int
nextLevelDelayTicks = 120

shipSize : Float
shipSize = 0.03

lifeSize : Float
lifeSize = 0.021

gameTextHeight : Float
gameTextHeight = shipSize

scoreTextHeight : Float
scoreTextHeight = shipSize

shipDebrisSize : Float
shipDebrisSize = 0.02

shipSizeForCollisions : Float
shipSizeForCollisions = shipSize * 0.4

turningSpeed : Float
turningSpeed = 0.1

thrust : Vec2
thrust = { x = 0, y = 0.00013 }

spaceFriction : Float
spaceFriction = 0.99

maxMomentum : Float
maxMomentum = 0.009

invincibleShipTime : Int
invincibleShipTime = 100

hyperspaceTime : Int
hyperspaceTime = 30

deadShipTime : Int
deadShipTime = 120

deadShipMomentumChange : Float
deadShipMomentumChange = 0.4

shotShipRatio : Float
shotShipRatio = 0.2

shotSize : Float
shotSize = shipSize * shotShipRatio

shotSpeed : Vec2
shotSpeed = { x = 0, y = 0.012 }

shotLifetime : Int
shotLifetime = 40

asteroidSpeedMin : Float
asteroidSpeedMin = 0.0005

asteroidSpeedBig : Float
asteroidSpeedBig = 0.001

asteroidSpeedMedium : Float
asteroidSpeedMedium = 0.002

asteroidSpeedSmall : Float
asteroidSpeedSmall = 0.003

asteroidSizeBig : Float
asteroidSizeBig = 0.04

asteroidSizeMedium : Float
asteroidSizeMedium = 0.02

asteroidSizeSmall : Float
asteroidSizeSmall = 0.01

asteroidScoreBig : Int
asteroidScoreBig = 20

asteroidScoreMedium : Int
asteroidScoreMedium = 50

asteroidScoreSmall : Int
asteroidScoreSmall = 100

explosionLifetime : Int
explosionLifetime = 30

explosionSpeed : Float
explosionSpeed = 0.001

explosionSize : Float
explosionSize = 0.021

debrisSize : Float
debrisSize = explosionSize / 10

saucerSizeBig : Float
saucerSizeBig = shipSize * 1.5

saucerSizeSmall : Float
saucerSizeSmall = saucerSizeBig / 2

saucerSizeCollisionRatio : Float
saucerSizeCollisionRatio = 0.4

saucerSpeedX : Float
saucerSpeedX = 0.002

saucerSpeedY : Float
saucerSpeedY = 0.0015

saucerDirectionTicksMin : Int
saucerDirectionTicksMin = 50

saucerDirectionTicksMax : Int
saucerDirectionTicksMax = 200

saucerShotTicks : Int
saucerShotTicks = 40

saucerScoreBig : Int
saucerScoreBig = 200

saucerScoreSmall : Int
saucerScoreSmall = 1000

saucerShotAccuracyRange : (Float, Float)
saucerShotAccuracyRange = (0.93, 0.999)

saucerTicksMin : Int
saucerTicksMin = 300

saucerTicksMax : Int
saucerTicksMax = 1000

maxDifficultyScore : Int
maxDifficultyScore = 40000

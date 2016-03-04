module Constants where

gameWidth = 1.0
gameHeight = 3.0 / 4.0

gameBoundsMinX = -gameWidth / 2
gameBoundsMaxX = gameWidth / 2

gameBoundsMinY = -gameHeight / 2
gameBoundsMaxY = gameHeight / 2

gameBoundsMin = { x = gameBoundsMinX, y = gameBoundsMinY }
gameBoundsMax = { x = gameBoundsMaxX, y = gameBoundsMaxY }
gameBounds = (gameBoundsMin, gameBoundsMax)

tickFps = 60
startLives = 3

extraLifeMultiple = 10000

startAsteroidCount = 8
introAsteroidCount = 30

shipSize = 0.03
shipSizeForCollisions = shipSize * 0.4

shipDebrisSize = 0.02
lifeSize = 0.021

gameTextHeight = shipSize
scoreTextHeight = shipSize

turningSpeed = 0.075
thrust = { x = 0, y = 0.00009 }
spaceFriction = 0.992
maxMomentum = 0.009

invincibleShipTicks = 100
nextLevelDelayTicks = 120
hyperspaceTicks = 30

deadShipTicks = 120
deadShipMomentumChange = 0.4

shotSize = shipSize * 0.2
shotSpeed = { x = 0, y = 0.012 }
shotLifetime = 40

asteroidSpeedMin = 0.0005
asteroidSpeedBig = 0.001
asteroidSpeedMedium = 0.002
asteroidSpeedSmall = 0.003

asteroidSizeBig = 0.04
asteroidSizeMedium = 0.02
asteroidSizeSmall = 0.01

asteroidScoreBig = 20
asteroidScoreMedium = 50
asteroidScoreSmall = 100

explosionLifetime = 30
explosionSpeed = 0.001
explosionSize = 0.021

debrisSize = explosionSize / 10

saucerSizeBig = shipSize * 1.5
saucerSizeSmall = saucerSizeBig / 2
saucerSizeCollisionRatio = 0.4

saucerSpeedX = 0.002
saucerSpeedY = 0.0015

saucerDirectionTicksMin = 50
saucerDirectionTicksMax = 200

saucerShotTicks = 40

saucerScoreBig = 200
saucerScoreSmall = 1000

saucerShotAccuracyRange = (0.75, 0.999)

saucerTicksMin = 300
saucerTicksMax = 1000

initialBigSaucerCount = 3

maxDifficultyScore = 40000

shipExplosionSound = "explosion-mid"
shipThrustSound = "thrust"
shipThrustSoundTicks = 6

fireSoundShip = "fire-ship"
fireSoundSaucer = "fire-saucer"

saucerSoundBig = "saucer-low"
saucerSoundBigTicks = 8
saucerSoundSmall = "saucer-high"
saucerSoundSmallTicks = 8

saucerExplosionSoundBig = "explosion-low"
saucerExplosionSoundSmall = "explosion-mid"

asteroidSoundBig = "explosion-low"
asteroidSoundMedium = "explosion-mid"
asteroidSoundSmall = "explosion-high"


module HeartBeat
  ( HeartBeat
  , defaultHeartBeat
  , resetHeartBeat
  , tick
  ) where

import Constants

type Beat
  = High
  | Low

type alias HeartBeat =
  { beat : Beat
  , nextBeatTickCount : Int
  , beatTicks : Int
  , tickCount : Int
  }


defaultHeartBeat : HeartBeat
defaultHeartBeat =
  { beat = High
  , tickCount = 0
  , nextBeatTickCount = 0
  , beatTicks = 0
  }


resetHeartBeat : HeartBeat -> Int -> HeartBeat
resetHeartBeat heartBeat level =
  let
    beatTicks = Constants.heartBeatMaxTicks - level * 2
  in
    { heartBeat
    | tickCount = 0
    , beatTicks = beatTicks
    , nextBeatTickCount = beatTicks
    }


tick : HeartBeat -> (HeartBeat, Maybe String)
tick heartBeat =
  let
    tickCount = heartBeat.tickCount + 1
  in
    if heartBeat.tickCount == heartBeat.nextBeatTickCount then
      let
        sound = case heartBeat.beat of
          High -> Constants.heartBeatSoundHigh
          Low -> Constants.heartBeatSoundLow
        beatTicks = case heartBeat.beat of
          High -> heartBeat.beatTicks
          Low -> max (heartBeat.beatTicks - 1) Constants.heartBeatMinTicks
        beat = case heartBeat.beat of
          High -> Low
          Low -> High
        heartBeat' =
          { heartBeat
          | tickCount = tickCount
          , beatTicks = beatTicks
          , nextBeatTickCount = heartBeat.tickCount + beatTicks
          , beat = beat
          }
      in
        (heartBeat', Just sound)
    else
      ({ heartBeat | tickCount = tickCount }, Nothing)

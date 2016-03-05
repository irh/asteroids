module Main (main) where

import Constants
import Effects
import Game
import Keyboard
import Primer
import Signal.Time
import StartApp
import Task
import Time
import View
import Window


inputSignals =
  [ Signal.map Game.Tick (Time.fps Constants.tickFps)
  , Signal.map Game.Arrows Keyboard.arrows
  , Signal.map Game.Wasd Keyboard.wasd
  , Signal.map Game.Space Keyboard.space
  , Signal.map Game.Escape (Keyboard.isDown 27)
  , Signal.map Game.KeyM (Keyboard.isDown 77)
  , Signal.map Game.Window (Primer.prime Window.dimensions)
  ]


app =
  StartApp.start
  { init = Game.initialGame
  , update = Game.updateGame
  , view = View.view
  , inputs = inputSignals
  }


main =
  app.html


port tasks : Signal (Task.Task Effects.Never ())
port tasks =
  app.tasks

port sounds : Signal String
port sounds =
  Game.soundSignal

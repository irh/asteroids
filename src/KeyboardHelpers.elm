module KeyboardHelpers
  (Arrows
  , defaultArrows
  ) where

type alias Arrows = { x : Int, y : Int }

defaultArrows : Arrows
defaultArrows = { x = 0, y = 0 }


module Lib (
    mainFunc
) where

import Graphics.Gloss
import Graphics.Gloss.Data.Color
import World

myDisplay = (InWindow "Hello World" (1300, 700) (500, 500))

grey = light black

mainFunc :: IO ()
mainFunc = display myDisplay grey $ color white $ render sampleWorld

module Lib (
    mainFunc
) where

import Graphics.Gloss
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import Data.List
import Boid (Boid)
import qualified Boid as Boid
import BoidWorld (BoidWorld)
import qualified BoidWorld as BoidWorld


myDisplay = (InWindow "Hello World" (700, 700) (500, 500))

mainFunc :: IO ()
mainFunc = play myDisplay white 60 BoidWorld.sample BoidWorld.render (const id) (const (BoidWorld.oneStep 50 350 350))

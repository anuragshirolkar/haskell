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
import qualified Simulation as Sim
import Config


myDisplay = (InWindow "Hello World" (1300, 700) (500, 500))

mainFunc :: IO ()
mainFunc = play myDisplay white 60 BoidWorld.sample BoidWorld.render (const id) (const (Sim.oneStep (Config 30 100 20 15 650 350)))

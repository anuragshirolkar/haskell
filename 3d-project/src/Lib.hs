module Lib (
    mainFunc
) where

import Graphics.Gloss
import Graphics.Gloss.Data.Color
import World
import Model
import Codec.Wavefront
import System.Directory
import Camera (initCam)
import Renderer

myDisplay = (InWindow "Hello World" (1300, 700) (500, 500))

grey = light black

mainFunc :: IO ()
mainFunc = 
    do
        obj <- teaPotObj
        let world = initWorld initCam obj
        play myDisplay grey 60 world render (World.handleEvent world) (const World.moveWorld)

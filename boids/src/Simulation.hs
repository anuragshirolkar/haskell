module Simulation (
    oneStep
) where

import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Vector
import Data.List (zipWith)
import qualified Util as Util
import Separation
import Alignment
import Cohesion
import Config
import BoidWorld
import Boid

applyForce :: (Float, Float) -> Float -> Float
applyForce (fx, fy) orient = newOrient
    where
        (dx, dy) =  unitVectorAtAngle (-orient)
        newDir = (dx + fx, dy + fy)
        newOrient = 2*pi - (argV newDir)

boidOneStep :: Config -> BoidWorld -> Boid -> Boid
boidOneStep (Config sr osr ar cr mx my) (BoidWorld allBoids obs) (Boid (x, y) orient clr spd) = Boid (nx, ny) newOrient clr spd
    where
        wallPoints = [(mx, y), (-mx, y), (x, my), (x, -my)]
        sepForce = separationForce sr (map boidPosition allBoids) (x, y)
        obsSepForce = separationForce osr (wallPoints ++ obs) (x,y)
        alignForce = alignmentForce ar allBoids (x,y)
        cohesForce = cohesionForce cr allBoids (x,y)
        newOrient = applyForce (Util.sumV [sepForce, obsSepForce, alignForce, cohesForce]) orient
        ny = flipAtBoundary my $ y - (sin newOrient) * spd
        nx = flipAtBoundary mx $ x + (cos newOrient) * spd

oneStep :: Config -> BoidWorld -> BoidWorld
oneStep config world = BoidWorld (map (boidOneStep config world) (worldBoids world)) (worldObstacles world)
                
flipAtBoundary :: Float -> Float -> Float
flipAtBoundary mx x
    | (abs x) > mx      = 0 - x
    | otherwise         = x
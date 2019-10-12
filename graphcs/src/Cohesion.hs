module Cohesion (
    cohesionForce
) where

import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Vector
import Boid
import qualified Util as Util

cohesionForce :: Float -> [Boid] -> Point -> Vector
cohesionForce r boids (x,y)
    | com == (x,y)              = (0,0)
    | otherwise                 = mulSV 0.02 comDir
    where
        cluster = filter (\b -> (Util.distance (x,y) (boidPosition b)) < r) boids
        com = Util.avgV $ map boidPosition boids
        comDir = normalizeV $ Util.diffV com (x,y)
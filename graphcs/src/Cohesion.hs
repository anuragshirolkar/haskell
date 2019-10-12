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
        boidPoints = map boidPosition boids
        weights = map (weight r . Util.distance (x,y)) boidPoints
        com = Util.avgV $ zipWith mulSV weights boidPoints
        comDir = normalizeV $ Util.diffV com (x,y)

weight :: Float -> Float -> Float
weight r dist
    | dist > r          = 0
    | otherwise         = 1 - dist/r
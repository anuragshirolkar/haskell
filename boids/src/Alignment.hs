module Alignment (
    alignmentForce
) where

import qualified Util as Util
import Boid
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Vector

alignmentForce :: Float -> [Boid] -> Point -> Vector
alignmentForce r boids (x,y) = mulSV 0.02 $ normalizeV alignment
    where
        cluster = filter (\b -> (Util.distance (x,y) (boidPosition b)) < r) boids
        alignment = Util.sumV $ map (unitVectorAtAngle . boidOrientation) boids
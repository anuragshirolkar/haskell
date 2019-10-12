module Separation (
    separationForce
) where

import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Data.Point
import qualified Util as Util

separationForce :: Float -> [Point] -> Point -> Vector
separationForce r obs (x,y) = force
    where
        relativeObs = filter (\x -> x /= (0,0)) $ map (flip Util.diffV (x,y)) obs
        forces = map (sepForceMag r . magV) relativeObs
        forceDirs = map (mulSV (-1.0) . normalizeV) relativeObs
        force = Util.sumV $ zipWith mulSV forces forceDirs

sepForceMag :: Float -> Float -> Float
sepForceMag r dist
    | dist > r      = 0
    | dist == 0     = 0
    | otherwise     = (10.0/dist - 10.0/r)

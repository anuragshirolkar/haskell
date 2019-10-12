module Boid (
    Boid (..), render, oneStep, sample
) where
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Vector
import Data.List (zipWith)
import qualified Util as Util

data Boid = Boid {
    boidPosition :: Point,
    boidOrientation :: Float,
    boidColor :: Color,
    boidSpeed :: Float
}

applyForce :: (Float, Float) -> Float -> Float
applyForce (fx, fy) orient = newOrient
    where
        (dx, dy) =  unitVectorAtAngle (-orient/180*pi)
        newDir = (dx + fx, dy + fy)
        newOrient = 360 - (argV newDir)*180/pi


oneStep :: Float -> Float -> Float -> [Boid] -> Boid -> Boid
oneStep r mx my allBoids (Boid (x, y) orient clr spd) = Boid (nx, ny) newOrient clr spd
            where
                wallPoints = [(mx, y), (-mx, y), (x, my), (x, -my)]
                sepForce = separationForce r (wallPoints ++ (map boidPosition allBoids)) (x, y)
                newOrient = applyForce sepForce orient
                theta = (newOrient / 180) * pi
                ny = flipAtBoundary my $ y - (sin theta) * spd
                nx = flipAtBoundary mx $ x + (cos theta) * spd

separationForce1 :: Float -> [Point] -> Point -> (Float, Float)
separationForce1 r obs (x, y) = sumForce
    where
        relativeObs = map (flip Util.diffV (x,y)) obs
        relativeObsF = filter (\v -> magV v < r && magV v > 0) relativeObs
        distances = map magV relativeObsF
        zipper (relObX, relObY) dist = (-5*relObX/dist/dist, -5*relObY/dist/dist)
        forces = zipWith zipper relativeObsF distances
        sumForce = (sum (map fst forces), sum (map snd forces))

separationForce :: Float -> [Point] -> Point -> Vector
separationForce r obs (x,y) = force
    where
        relativeObs = filter (\x -> x /= (0,0)) $ map (flip Util.diffV (x,y)) obs
        forces = map (sepForceMag r . magV) relativeObs
        forceDirs = map (mulSV (-1.0) . normalizeV) relativeObs
        force = Util.sumAllV $ zipWith mulSV forces forceDirs

sepForceMag :: Float -> Float -> Float
sepForceMag r dist
    | dist > r      = 0
    | dist == 0     = 0
    | otherwise     = (10.0/dist - 10.0/r)

                
flipAtBoundary :: Float -> Float -> Float
flipAtBoundary mx x
    | (abs x) > mx      = 0 - x
    | otherwise         = x

render :: Boid -> Picture
render (Boid (x, y) rot clr spd) = color clr $ translate x y $ rotate rot $ Pictures [mainBoid, blur]
    where
        mainBoid = Polygon [(10,0), (-10,5), (-10,-5)]
        blur = translate (-spd/2) 0 mainBoid

sample :: Int -> Boid
sample angle = Boid (0,0) (fromIntegral angle) (Util.makeColorS "0077c2") 5
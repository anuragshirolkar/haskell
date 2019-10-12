module Boid (
    Boid (..), render, sample
) where
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
import qualified Util as Util

data Boid = Boid {
    boidPosition :: Point,
    boidOrientation :: Float,
    boidColor :: Color,
    boidSpeed :: Float
}

render :: Boid -> Picture
render (Boid (x, y) rot clr spd) = color clr $ translate x y $ rotate (Util.toDeg rot) $ Pictures [mainBoid, blur]
    where
        mainBoid = Polygon [(10,0), (-10,5), (-10,-5)]
        blur = translate (-spd/2) 0 mainBoid

sample :: Int -> Boid
sample angle = Boid (0,0) (Util.toRad (fromIntegral angle)) (Util.makeColorS "0077c2") 3
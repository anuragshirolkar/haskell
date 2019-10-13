module BoidWorld (
    BoidWorld (..), render, sample
) where
import Boid (Boid)
import qualified Boid as Boid
import Graphics.Gloss.Data.Picture
import Graphics.Gloss (black)

data BoidWorld = BoidWorld {
    worldBoids :: [Boid],
    worldObstacles :: [Point]
}

render :: BoidWorld -> Picture
render (BoidWorld boids obstacles) = Pictures $ (map Boid.render boids) ++ (map renderObstacle obstacles)

renderObstacle :: Point -> Picture
renderObstacle (x,y) = translate x y $ color black $ thickCircle 0 10

obs = [(100,100), (-100,-100), (200,-100), (-200, 100), (300, 100), (-300, -100), (400, -100), (-400, 100)]

sample = BoidWorld (map Boid.sample [0,1..89]) obs
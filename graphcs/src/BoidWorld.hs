module BoidWorld (
    BoidWorld (..), render, oneStep, sample
) where
import Boid (Boid)
import qualified Boid as Boid
import Graphics.Gloss.Data.Picture

data BoidWorld = BoidWorld {
    worldBoids :: [Boid]
}

render :: BoidWorld -> Picture
render (BoidWorld boids) = Pictures $ map Boid.render boids

oneStep :: Float -> Float -> Float -> BoidWorld -> BoidWorld
oneStep r mx my (BoidWorld boids) = BoidWorld $ map mapper boids
    where mapper = Boid.oneStep r mx my boids

sample = BoidWorld $ map Boid.sample [0,2..88]
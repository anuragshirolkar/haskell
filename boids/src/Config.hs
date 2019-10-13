module Config (
    Config (..)
) where

data Config = Config {
    separationR :: Float,
    obsSeparationR :: Float,
    alignmentR :: Float,
    cohesionR :: Float,
    maxX :: Float,
    maxY :: Float
}
module Camera (
    Camera (..), makeImage, sampleCam
) where

import Point3
import Data.Matrix (Matrix)
import qualified Data.Matrix as Matrix
import Graphics.Gloss.Data.Point (Point)
import qualified Data.Vector as Vector

data Camera = Camera {
    camMatrix :: Matrix Float
} deriving (Show)

perspectiveM :: Matrix Float
perspectiveM = Matrix.fromList 3 4 [1000,0,0,0, 0,1000,0,0, 0,0,1,0]

goBack :: Float -> Camera -> Camera
goBack dist cam = translateCam (0,0,(-dist)) cam

goUp :: Float -> Camera -> Camera
goUp dist cam = translateCam (0,dist,0) cam

translateCam :: Point3 -> Camera -> Camera
translateCam (x,y,z) (Camera mat) = Camera $ Matrix.multStd newMat mat
    where
        newMat = Matrix.fromList 4 4 [1,0,0,(-x), 0,1,0,(-y), 0,0,1,(-z), 0,0,0,1]

sampleCam :: Camera
sampleCam = goUp 100 $ goBack 1000 $ (Camera (Matrix.identity 4))

makeImage :: Camera -> Point3 -> Point
makeImage (Camera mat) (x,y,z) = (x'/z', y'/z')
    where
        pointMat = Matrix.fromList 4 1 [x, y, z, 1]
        [x',y',z'] = Vector.toList $ Matrix.getCol 1 $ Matrix.multStd perspectiveM (Matrix.multStd mat pointMat)

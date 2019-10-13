module Camera (
    Camera (..), makeImage, sampleCam, translate, makeMovement
) where

import Point3
import Data.Matrix (Matrix)
import qualified Data.Matrix as Matrix
import Graphics.Gloss.Data.Point (Point)
import qualified Data.Vector as Vector
import Events (Movement)
import qualified Events

data Camera = Camera {
    camMatrix :: Matrix Float
} deriving (Show)

perspectiveM :: Matrix Float
perspectiveM = Matrix.fromList 3 4 [1000,0,0,0, 0,1000,0,0, 0,0,1,0]

goBack :: Float -> Camera -> Camera
goBack dist cam = translate (0,0,(-dist)) cam

goUp :: Float -> Camera -> Camera
goUp dist cam = translate (0,dist,0) cam

sampleCam :: Camera
sampleCam = goUp 100 $ goBack 1000 $ (Camera (Matrix.identity 4))

makeImage :: Camera -> Point3 -> Point
makeImage (Camera mat) (x,y,z) = (x'/z', y'/z')
    where
        pointMat = Matrix.fromList 4 1 [x, y, z, 1]
        [x',y',z'] = Vector.toList $ Matrix.getCol 1 $ Matrix.multStd perspectiveM (Matrix.multStd mat pointMat)



translate :: Point3 -> Camera -> Camera
translate (x,y,z) (Camera mat) = Camera $ Matrix.multStd newMat mat
    where
        newMat = Matrix.fromList 4 4 [1,0,0,(-x), 0,1,0,(-y), 0,0,1,(-z), 0,0,0,1]

-- angle in radians, positive when camera turns left
turnHorizontal :: Float -> Camera -> Camera
turnHorizontal theta (Camera mat) = Camera $ Matrix.multStd newMat mat
    where
        newMat = Matrix.fromList 4 4 [cos theta,0,sin theta,0, 0,1,0,0, -(sin theta),0,cos theta,0, 0,0,0,1]


translateUnit = 5
turnUnit = 0.01

makeMovement :: Camera -> Movement -> Camera
makeMovement cam Events.MoveForward = translate (0,0,translateUnit) cam
makeMovement cam Events.MoveBackward = translate (0,0,-translateUnit) cam
makeMovement cam Events.MoveLeft = translate (-translateUnit,0,0) cam
makeMovement cam Events.MoveRight = translate (translateUnit,0,0) cam
makeMovement cam Events.MoveUp = translate (0,translateUnit,0) cam
makeMovement cam Events.MoveDown = translate (0,-translateUnit,0) cam
makeMovement cam Events.TurnLeft = turnHorizontal turnUnit cam
makeMovement cam Events.TurnRight = turnHorizontal (-turnUnit) cam
makeMovement cam _ = cam
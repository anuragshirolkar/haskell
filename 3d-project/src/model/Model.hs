module Model (
    teaPotObj, testPrintObj, cubeObj, objToPaths
) where

import Codec.Wavefront
import Data.Either
import qualified Data.Vector as Vector
import Point3

teaPotPath = "src/model/teapot.obj"

teaPotObj :: IO WavefrontOBJ
teaPotObj = fmap getObj $ fromFile teaPotPath

cubePath = "src/model/cube.obj"
cubeObj :: IO WavefrontOBJ
cubeObj = fmap getObj $ fromFile cubePath

getObj :: Either String WavefrontOBJ -> WavefrontOBJ
getObj (Left s) = WavefrontOBJ Vector.empty Vector.empty Vector.empty Vector.empty Vector.empty Vector.empty Vector.empty
getObj (Right w) = w

testPrintObj :: WavefrontOBJ -> String
testPrintObj (WavefrontOBJ _ _ _ _ _ faces _) = show $ Vector.length faces

faceToPath :: Vector.Vector Location -> Face -> [Point3]
faceToPath locations (Face i1 i2 i3 is) = map (fromLocation . (Vector.!) locations . flip (-) 1 . faceLocIndex) (i1:i2:i3:is)

objToPaths :: WavefrontOBJ -> [[Point3]]
objToPaths obj = paths
    where
        locations = objLocations obj
        paths = Vector.toList $ Vector.map (faceToPath locations . elValue) $ objFaces obj
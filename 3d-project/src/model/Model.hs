module Model (
    Model (..),    
    teaPotObj, testPrintObj, cubeObj, objToPaths, renderModel
) where

import qualified Codec.Wavefront as WF
import Data.Either
import qualified Data.Vector as Vector
import Point3
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Point

data Model = Model {
    modelShape :: WF.WavefrontOBJ,
    modelScale :: Float
}

renderModel :: (Point3 -> Point) -> Model -> Picture
renderModel projFn (Model obj s) = renderWFObj projFn obj s

renderWFObj :: (Point3 -> Point) -> WF.WavefrontOBJ -> Float -> Picture
renderWFObj projFn obj s = Pictures $ map (Line . map (projFn . mulSV3 s)) $ objToPaths obj

readModel :: String -> Float -> IO Model
readModel filePath scale =
    do
        obj <- fmap getObj $ WF.fromFile filePath
        return $ Model obj scale

teaPotObj = readModel "src/model/teapot.obj" 50
cubeObj = readModel "src/model/cube.obj"

getObj :: Either String WF.WavefrontOBJ -> WF.WavefrontOBJ
getObj (Left s) = WF.WavefrontOBJ Vector.empty Vector.empty Vector.empty Vector.empty Vector.empty Vector.empty Vector.empty
getObj (Right w) = w

testPrintObj :: WF.WavefrontOBJ -> String
testPrintObj (WF.WavefrontOBJ _ _ _ _ _ faces _) = show $ Vector.length faces

faceToPath :: Vector.Vector WF.Location -> WF.Face -> [Point3]
faceToPath locations (WF.Face i1 i2 i3 is) = map (fromLocation . (Vector.!) locations . flip (-) 1 . WF.faceLocIndex) (i1:i2:i3:is)

objToPaths :: WF.WavefrontOBJ -> [[Point3]]
objToPaths obj = paths
    where
        locations = WF.objLocations obj
        paths = Vector.toList $ Vector.map (faceToPath locations . WF.elValue) $ WF.objFaces obj
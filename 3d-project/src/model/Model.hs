module Model (
    Model (..),    
    teaPotObj, cubeObj
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
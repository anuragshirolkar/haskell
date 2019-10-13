module Renderer (
    render
) where

import Model
import qualified Codec.Wavefront as WF
import qualified Data.Vector as Vector
import Point3
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import Camera
import World


renderModel :: (Point3 -> Point) -> Model -> Picture
renderModel projFn (Model obj s) = renderWFObj projFn obj s

renderWFObj :: (Point3 -> Point) -> WF.WavefrontOBJ -> Float -> Picture
renderWFObj projFn obj s = Pictures $ map (Line . map (projFn . mulSV3 s)) $ objToPaths obj

objToPaths :: WF.WavefrontOBJ -> [[Point3]]
objToPaths obj = paths
    where
        locations = WF.objLocations obj
        paths = Vector.toList $ Vector.map (faceToPath locations . WF.elValue) $ WF.objFaces obj

faceToPath :: Vector.Vector WF.Location -> WF.Face -> [Point3]
faceToPath locations (WF.Face i1 i2 i3 is) = map (fromLocation . (Vector.!) locations . flip (-) 1 . WF.faceLocIndex) (i1:i2:i3:is)


render :: World -> Picture
render (World cam obj _) = Color white $ renderModel projFn obj
    where
        projFn = Camera.makeImage cam
module World (
    World (..), initWorld, render, handleEvent, moveWorld
) where

import Point3
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Picture
import Camera (Camera)
import qualified Camera as Camera
import Events (Movement, Action)
import qualified Events as Events
import Graphics.Gloss.Interface.IO.Interact
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Codec.Wavefront as WF
import Model
import qualified Data.Vector as Vector

data World = World {
    worldCamera :: Camera,
    worldObject :: Object,
    worldMovements :: Set Movement
}

data Object = 
    Cube Point3 Float
    | Wavefront WF.WavefrontOBJ

renderObject :: (Point3 -> Point) -> Object -> Picture
renderObject projFn (Wavefront obj) = renderWFObj projFn obj
renderObject projFn (Cube (x,y,z) l) = Pictures [l200,l210,l211,l201,l020,l120,l121,l021,l002,l102,l112,l012]
    where
        [p000,p100,p110,p010,p001,p101,p111,p011] = map (projFn . addV3 (x,y,z) . mulSV3 (l/2)) [(-1,-1,-1), (1,-1,-1), (1,1,-1), (-1,1,-1), (-1,-1,1), (1,-1,1), (1,1,1), (-1,1,1)]
        l200 = Line [p000,p100]
        l210 = Line [p010,p110]
        l211 = Line [p011,p111]
        l201 = Line [p001,p101]
        l020 = Line [p000,p010]
        l120 = Line [p100,p110]
        l121 = Line [p101,p111]
        l021 = Line [p001,p011]
        l002 = Line [p000,p001]
        l102 = Line [p100,p101]
        l112 = Line [p110,p111]
        l012 = Line [p110,p011]

renderWFObj :: (Point3 -> Point) -> WF.WavefrontOBJ -> Picture
renderWFObj projFn obj = Pictures $ map (Polygon . map projFn) $ objToPaths obj

initWorld :: Camera -> WF.WavefrontOBJ -> World
initWorld cam obj = World cam (Wavefront obj) Set.empty

render :: World -> Picture
render (World cam cube _) = Color white $ renderObject projFn cube
    where
        projFn = Camera.makeImage cam

recordMovement :: World -> Action -> World -> World
recordMovement _ (Events.Add movement) (World c o movements) = World c o (Set.insert movement movements)
recordMovement _ (Events.Remove movement) (World c o movements) = World c o (Set.delete movement movements)
recordMovement worldReset Events.Reset _ = worldReset
recordMovement _ _ world = world

handleEvent :: World -> Event -> World -> World
handleEvent worldReset event = recordMovement worldReset (Events.fromEvent event)

moveWorld :: World -> World
moveWorld (World cam object movements) = World (Set.foldl Camera.makeMovement cam movements) object movements
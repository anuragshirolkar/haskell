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

data Object = Wavefront WF.WavefrontOBJ

renderObject :: (Point3 -> Point) -> Object -> Picture
renderObject projFn (Wavefront obj) = renderWFObj projFn obj

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
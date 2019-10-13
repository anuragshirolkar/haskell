module World (
    World (..), initWorld, handleEvent, moveWorld
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
    worldObject :: Model,
    worldMovements :: Set Movement
}

initWorld :: Camera -> Model -> World
initWorld cam obj = World cam obj Set.empty

recordMovement :: World -> Action -> World -> World
recordMovement _ (Events.Add movement) (World c o movements) = World c o (Set.insert movement movements)
recordMovement _ (Events.Remove movement) (World c o movements) = World c o (Set.delete movement movements)
recordMovement worldReset Events.Reset _ = worldReset
recordMovement _ _ world = world

handleEvent :: World -> Event -> World -> World
handleEvent worldReset event = recordMovement worldReset (Events.fromEvent event)

moveWorld :: World -> World
moveWorld (World cam object movements) = World (Set.foldl Camera.makeMovement cam movements) object movements
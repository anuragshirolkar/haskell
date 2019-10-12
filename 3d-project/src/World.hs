module World (
    World (..), sampleWorld, render
) where

import Point3
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Picture
import Camera

data World = World {
    camera :: Camera,
    object :: Object
}

data Object = Cube Point3 Float

renderObject :: (Point3 -> Point) -> Object -> Picture
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
        l012 = Line [p010,p011] 

sampleWorld = World sampleCam (Cube (0,0,0) 100)

render :: World -> Picture
render (World cam cube) = renderObject projFn cube
    where
        projFn = makeImage sampleCam
module Util (
    makeColorS, diffV, sumAllV
) where

import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Point

import Numeric (readHex)

makeColorS :: String -> Color
makeColorS [r1,r2,g1,g2,b1,b2] = makeColorI (readHex' [r1, r2]) (readHex' [g1, g2]) (readHex' [b1, b2]) 255
    where readHex' = fst . head . readHex

distance :: (Float, Float) -> (Float, Float) -> Float
distance (x1,y1) (x2,y2) = sqrt (dx*dx + dy*dy)
    where
        dx = x1-x2
        dy = y1-y2

diffV :: (Float, Float) -> (Float, Float) -> (Float, Float)
diffV (x1,y1) (x2,y2) = (x1-x2, y1-y2)

sumV :: Point -> Point -> Point
sumV (x1,y1) (x2, y2) = (x1+x2, y1+y2)

sumAllV :: [Point] -> Point
sumAllV = foldl sumV (0,0)


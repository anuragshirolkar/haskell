module Point3 (
    Point3, Vector3, mulSV3, addV3
) where

type Point3 = (Float, Float, Float)

type Vector3 = Point3

mulSV3 :: Float -> Vector3 -> Vector3
mulSV3 m (x, y, z) = (m*x, m*y, m*z)

addV3 :: Vector3 -> Vector3 -> Vector3
addV3 (x1,y1,z1) (x2,y2,z2) = (x1+x2, y1+y2, z1+z2)
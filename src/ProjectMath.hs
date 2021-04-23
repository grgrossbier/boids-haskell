module ProjectMath where

import Linear.V2
import Linear.V1
import Linear.Metric
import Linear.Matrix

scale :: Num a => a -> V2 a -> V2 a
scale a v = (a*) <$> v

xOnly :: V2 a -> a
xOnly (V2 x _) = x

yOnly :: V2 a -> a
yOnly (V2 _ y) = y

fmapIf :: Functor f => (b -> Bool) -> (b -> b) -> f b -> f b
fmapIf p f = fmap (\x -> if p x then f x else x)

rotateV2 :: (Floating a, Num a) => V2 a -> a -> V2 a
rotateV2 v a = rotated
  where
    sa = sin a
    ca = cos a
    rotationMatrix = V2 (V2 ca (-sa)) (V2 sa ca)
    (V1 rotated) = V1 v !*! (transpose rotationMatrix)


-- https://cs.stackexchange.com/questions/127295/algorithm-for-intersection-point-between-two-vectors
-- intersectionOfLines
--     d1 = cross(p - a, b - a)
--     d2 = cross(q - a, b - a)
--     return (d1 * q - d2 * p) / (d1 - d2)


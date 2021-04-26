module ProjectMath where

import Shapes 

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

-- | Rotate V2 Vector CCW by angle in radians.
rotateV2 :: (Floating a, Num a) => V2 a -> a -> V2 a
rotateV2 v a = rotated
  where
    sa = sin a
    ca = cos a
    rotationMatrix = V2 (V2 ca (-sa)) (V2 sa ca)
    (V1 rotated) = V1 v !*! (transpose rotationMatrix)

-- | Keep within 0 to 2pi
limitAngle :: Angle -> Angle
limitAngle angle
    | angle < 0     = until (>0) (+(2*pi)) angle
    | angle >= 2*pi = until (<2*pi) (+(-2*pi)) angle
    | otherwise     = angle

-- | Keep within angle range. 
--
-- If `limitTurn pi -pi`, then it just remaps the turn onto a new range.
-- If `limitTurn pi/2 -pi/2` then it will prevent over turning on any single iteration.
limitTurn :: Angle -> Angle -> Angle -> Angle
limitTurn upper lower angle = limitToBounds
  where
    zeroTo2Pi = limitAngle angle
    piToNegPi = if zeroTo2Pi > pi 
                then zeroTo2Pi - 2*pi 
                else zeroTo2Pi
    limitToBounds = max lower $ min upper piToNegPi

simpleLimit :: Angle -> Angle
simpleLimit = limitTurn (pi/2) (-pi/2)

vectorFromAtoB :: (Float, Float) -> (Float, Float) -> (Float, Float)
vectorFromAtoB (x, y) (x', y') = (x'-x, y'-y)

vectorMagnitude :: (Float, Float) -> Float
vectorMagnitude (x, y) = (x**2 + y**2) ** (1/2)
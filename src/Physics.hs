module Physics where

import Numeric

import Shapes
import Globals
import Rendering


vectorFromAtoB :: (Float, Float) -> (Float, Float) -> (Float, Float)
vectorFromAtoB (x, y) (x', y') = (x'-x, y'-y)

vectorMagnitude :: (Float, Float) -> Float
vectorMagnitude (x, y) = (x**2 + y**2) ** (1/2)

splitVectorIntoComponents :: Float -> (Float, Float) -> (Float, Float)
splitVectorIntoComponents mag (nx, ny) = ( mag * cos angle , mag * sin angle )
  where
    angle = (pi/2) - atan2 nx ny

springPullOnShape :: (Float, Float) -> Shape -> Shape
springPullOnShape sPos s = s { sAcceleration = forceVectors}
  where
    pos = sPosition s
    toCenter = vectorFromAtoB pos sPos
    dist = vectorMagnitude toCenter 
    force = kSpring * dist
    forceVectors = splitVectorIntoComponents force toCenter

advanceTime :: Float -> Enviornment -> Enviornment 
advanceTime dt env = reactToObsticles
  where
    applyForces = applyForceToShapes env
    advanceShapes = advanceAllShapes dt applyForces
    reactToObsticles = reactToAllObsticles dt advanceShapes

applyForceToShapes :: Enviornment -> Enviornment 
applyForceToShapes env = env { eShapes = forcesApplied }
  where
    shapes = eShapes env
    center = eCenter env
    forcesApplied = map (springPullOnShape center) shapes

advanceAllShapes :: Float -> Enviornment -> Enviornment 
advanceAllShapes dt env = env { eShapes = advancedShapes}
  where
    shapes = eShapes env
    advancedShapes = map (advanceShape dt) shapes

advanceShape :: Float -> Shape -> Shape
advanceShape dt shape = shape 
    { sPosition = p2
    , sVelocity = v2 }
  where
    (px1, py1) = sPosition shape
    (vx1, vy1) = sVelocity shape
    (ax1, ay1) = sAcceleration shape
    fr = fromIntegral frameRate
    v2@(vx2, vy2) = (vx1 + ax1*dt , vy1 + ay1*dt)
    -- vAVG@(avx, avy) = ( (vx1+vx2)/2 , (vy1+vy2)/2 )
    p2 = (px1 + vx2*dt , py1 + vy2*dt)

reactToAllObsticles :: Float -> Enviornment -> Enviornment 
reactToAllObsticles dt env = env

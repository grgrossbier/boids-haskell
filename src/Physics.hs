module Physics where

import Numeric

import Shapes
import Globals
import Rendering
import Linear.V2
import Linear.Metric
import ProjectMath


vectorFromAtoB :: (Float, Float) -> (Float, Float) -> (Float, Float)
vectorFromAtoB (x, y) (x', y') = (x'-x, y'-y)

vectorMagnitude :: (Float, Float) -> Float
vectorMagnitude (x, y) = (x**2 + y**2) ** (1/2)

-- splitVectorIntoComponents :: Float -> (Float, Float) -> (Float, Float)
-- splitVectorIntoComponents mag (nx, ny) = ( mag * cos angle , mag * sin angle )
--   where
--     angle = (pi/2) - atan2 nx ny

setAccelerationToZero :: Shape -> Shape
setAccelerationToZero shape = shape { sAcceleration = V2 0 0 }

applySpringForce :: V2 Float -> Shape -> V2 Float
applySpringForce sPos s = dxy * kSpring
  where
    pos = sPosition s
    toCenter = sPos - pos
    nCenter = normalize toCenter
    dxy = toCenter - scale 100 nCenter 

applyGravity :: Shape -> V2 Float
applyGravity s = gravity 

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
    resetAccelerations = map setAccelerationToZero shapes
    forcesApplied = map (applyForceToShape env) resetAccelerations

applyForceToShape :: Enviornment -> Shape -> Shape
applyForceToShape env shape = forcesApplied
  where
    forces = 
        [ applySpringForce (eCenter env)
        , applyGravity]
        <*> [shape]
    totalForce = sum forces
    forcesApplied = shape { sAcceleration  = sAcceleration shape + totalForce }

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
    p1 = sPosition shape
    v1 = sVelocity shape
    a1 = sAcceleration shape
    v2 = v1 + scale dt a1
    p2 = p1 + scale dt v2

reactToAllObsticles :: Float -> Enviornment -> Enviornment 
reactToAllObsticles dt env = env

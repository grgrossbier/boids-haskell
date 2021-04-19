module Scratch where

import Linear.V2
import Globals
import Physics
import ProjectMath
import Rendering
import Shapes
import Player

sp1 :: V2 Float
sp1 = V2 10 1
sa1 :: V2 Float
sa1 = V2 10 0
sd1 :: V2 Float
sd1 = V2 (-1) 0
sv1 :: V2 Float
sv1 = V2 (-5) (-5)

shapeSteps :: [Shape]
shapeSteps = iterate oneShapeStep simpleCircle 

oneShapeStep :: Shape -> Shape
oneShapeStep shape = head $ eShapes react
  where
    dt = 0.1
    zerod = setAccelerationToZero shape
    forced = applyForceToShape newEnviornment zerod
    adv = advanceShape dt forced
    updateEnv = newEnviornment { eShapes = [adv] }
    react = reactToAllWalls dt 0.99 updateEnv

runWithOutGraphics :: [Enviornment]
runWithOutGraphics = iterate (advanceTime 0.02) newEnviornment 
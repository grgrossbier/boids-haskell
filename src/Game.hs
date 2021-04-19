module Game where

import Shapes
import Linear.V2
import System.Random

addShape :: V2 Float -> Enviornment -> Enviornment 
addShape pos env = env
    { eShapes = newShape : shapes
    , eStdGen = newGen'' }
  where
    shapes = eShapes env
    (newRadius, newGen) = randomR (5,30) (eStdGen env)
    (newVX, newGen') = randomR (-100,100) newGen
    (newVY, newGen'') = randomR (-100,100) newGen'
    newShape = simpleCircle
        { sPosition = pos
        , sGeometry = Circle newRadius
        , sVelocity = V2 newVX newVY}

addObsticle :: V2 Float -> Enviornment -> Enviornment 
addObsticle pos env = env
    { eObsticles = newOb : obst }
  where
    obst = eObsticles env
    newOb = centerCircle { sPosition = pos }

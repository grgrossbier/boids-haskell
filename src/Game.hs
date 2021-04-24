module Game where

import Shapes
import Linear.V2
import System.Random

addObsticle :: V2 Float -> Enviornment -> Enviornment 
addObsticle pos env = env
    { eObsticles = newObst : obsts
    , eStdGen = newGen'' }
  where
    obsts = eObsticles env
    (newRadius, newGen) = randomR (5,30) (eStdGen env)
    (newVX, newGen') = randomR (-100,100) newGen
    (newVY, newGen'') = randomR (-100,100) newGen'
    newObst = simpleCircle
        { sPosition = pos
        , sGeometry = Circle newRadius
        , sVelocity = V2 newVX newVY}

addShape :: V2 Float -> Enviornment -> Enviornment 
addShape pos env = env
    { eShapes = newShape : shapes 
    , eStdGen = newGen }
  where
    shapes = eShapes env
    (newAngle, newGen) = randomR (-pi, pi) (eStdGen env)
    newShape = simpleTriangle
        { sPosition = pos
        , sAngle = newAngle}    

addBird :: V2 Float -> Enviornment -> Enviornment 
addBird (V2 x y) env = env
    { eBirds = newBird : birds
    , eStdGen = newGen'' }
  where
    birds = eBirds env
    (newAngle, newGen) = randomR (0,2*pi) (eStdGen env)
    (dx, newGen') = randomR ((-3.0),3.0) newGen
    (dy, newGen'') = randomR ((-3.0),3.0) newGen'
    newBird = simpleTriangle 
        { sPosition = V2 (x+dx) (y+dy)
        , sAngle = newAngle}
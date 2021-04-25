module Game where

import Shapes
import Linear.V2
import System.Random

import Globals
import ProjectMath

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

killBirds :: Enviornment -> Enviornment
killBirds env = env { eBirds = [] }

removeObsticles :: Enviornment -> Enviornment
removeObsticles env = env { eObsticles = leftWall ++ rightWall ++ topWall ++ bottomWall }

removeShapes :: Enviornment -> Enviornment
removeShapes env = env { eShapes = [] }

scaleKSep :: Float -> Enviornment -> Enviornment 
scaleKSep scale env = env { eKSeparation = eKSeparation env * scale}

scaleKCoh :: Float -> Enviornment -> Enviornment 
scaleKCoh scale env = env { eKCohesion = eKCohesion env * scale}

scaleKAlign :: Float -> Enviornment -> Enviornment 
scaleKAlign scale env = env { eKAlignment = eKAlignment env * scale}

scaleKAvoid :: Float -> Enviornment -> Enviornment 
scaleKAvoid scale env = env { eKAvoidance = eKAvoidance env * scale}


-- Ball Border

borderBall = simpleCircle { sGeometry = Shapes.Circle 12 }
moveUp ball = ball { sPosition = sPosition ball + V2 0 30 }
moveRight ball = ball { sPosition = sPosition ball + V2 30 0 }

leftWall :: [Shape]
leftWall = 
    takeWhile (\b -> yOnly (sPosition b) < fromIntegral screenHeight)
    $ iterate moveUp 
    $ borderBall { sPosition = V2 15 30 }

rightWall :: [Shape]
rightWall = 
    takeWhile (\b -> yOnly (sPosition b) < fromIntegral screenHeight)
    $ iterate moveUp 
    $ borderBall { sPosition = V2 (fromIntegral screenWidth - 15) 30 }

bottomWall :: [Shape]
bottomWall = 
    takeWhile (\b -> xOnly (sPosition b) < (fromIntegral screenWidth - 30))
    $ iterate moveRight 
    $ borderBall { sPosition = V2 30 15 }

topWall :: [Shape]
topWall = 
    takeWhile (\b -> xOnly (sPosition b) < (fromIntegral screenWidth - 30))
    $ iterate moveRight 
    $ borderBall { sPosition = V2 30 (fromIntegral screenHeight - 15) }
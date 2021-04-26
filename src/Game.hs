module Game where

import Shapes
import Linear.V2
import System.Random
import qualified Graphics.Gloss as G

import Globals
import ProjectMath


-- | Add Obsticle at location. Circle with random velocity and radius.
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

-- | Add Shape at location. Circle with random velocity and radius.
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

-- | Add Bird (Triangle) at location. Random Delta to location to avoid stacking.
-- Random starting angle. 
addBird :: V2 Float -> Enviornment -> Enviornment 
addBird (V2 x y) env = env
    { eBirds = newBird : birds
    , eStdGen = newGen''''' }
  where
    birds = eBirds env
    (newAngle, newGen) = randomR (0,2*pi) (eStdGen env)
    (dx, newGen') = randomR ((-3.0),3.0) newGen
    (dy, newGen'') = randomR ((-3.0),3.0) newGen'
    colorRange = (50,255)
    (rRGB, newGen''') = randomR colorRange newGen''
    (gRGB, newGen'''') = randomR colorRange newGen'''
    (bRGB, newGen''''') = randomR colorRange newGen''''
    newBird = simpleTriangle 
        { sPosition = V2 (x+dx) (y+dy)
        , sAngle = newAngle
        , sColor = G.makeColorI rRGB gRGB bRGB 255}


-- <><><><><><><><><><>  Empty Env Buckets  <><><><><><><><><><> --

killBirds :: Enviornment -> Enviornment
killBirds env = env { eBirds = [] }

removeObsticles :: Enviornment -> Enviornment
removeObsticles env = env { eObsticles = leftWall ++ rightWall ++ topWall ++ bottomWall }

removeShapes :: Enviornment -> Enviornment
removeShapes env = env { eShapes = [] }


-- <><><><><><><><><><>  K Constant Adjustments  <><><><><><><><><><> --

scaleKSep :: Float -> Enviornment -> Enviornment 
scaleKSep scale env = env { eKSeparation = eKSeparation env * scale}

scaleKCoh :: Float -> Enviornment -> Enviornment 
scaleKCoh scale env = env { eKCohesion = eKCohesion env * scale}

scaleKAlign :: Float -> Enviornment -> Enviornment 
scaleKAlign scale env = env { eKAlignment = eKAlignment env * scale}

scaleKAvoid :: Float -> Enviornment -> Enviornment 
scaleKAvoid scale env = env { eKAvoidance = eKAvoidance env * scale}


-- <><><><><><><><><><>  BALL BORDER  <><><><><><><><><><> --

borderBall :: Shape
borderBall = simpleCircle { sGeometry = Shapes.Circle 12 }

moveUp :: Shape -> Shape
moveUp ball = ball { sPosition = sPosition ball + V2 0 30 }

moveRight :: Shape -> Shape
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


-- <><><><><><><><><><>  Starter Birds  <><><><><><><><><><> --

testBirds :: [Shape]
testBirds = [bird1, bird2, bird3, bird4]

bird1 :: Shape
bird1 = simpleTriangle { sAngle = 0, sPosition = V2 200 200 }
bird2 :: Shape
bird2 = simpleTriangle { sAngle = degToRadian 10, sPosition = V2 205 200 }
bird3 :: Shape
bird3 = simpleTriangle { sAngle = degToRadian 20, sPosition = V2 200 205 }
bird4 :: Shape
bird4 = simpleTriangle { sAngle = degToRadian 30, sPosition = V2 205 205 }


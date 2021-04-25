module Player where

import qualified Graphics.Gloss as G
import Graphics.Gloss.Interface.Pure.Game

import Shapes
import Globals
import Rendering
import Physics
import Linear.V2
import System.Random
import Game
import ProjectMath


newEnviornment = 
    Enviornment 
    testBirds
    [ ] 
    (leftWall ++ rightWall ++ topWall ++ bottomWall)
    centerOfMap
    (mkStdGen 10)
    10 1 1 20

window = G.InWindow "Functional" (screenWidth, screenHeight) (100, 100)
backgroundColor = G.makeColorI 0 0 0 255

playWithGraphics :: IO ()
playWithGraphics = do
    randGen <- randomIO :: IO Int
    G.play 
        window 
        backgroundColor 
        frameRate
        newEnviornment { eStdGen = mkStdGen randGen }
        gameAsPicture 
        transformGame 
        advanceTime

transformGame :: Event -> Enviornment -> Enviornment 
transformGame (EventKey (MouseButton LeftButton) Down _ (mx, my)) env = 
    addBird ((V2 mx my) + globalOffset) env
transformGame (EventKey (MouseButton RightButton) Down _ (mx, my)) env = 
    addObsticle ((V2 mx my) + globalOffset) env
transformGame (EventKey (MouseButton MiddleButton) Down _ (mx, my)) env = 
    addShape ((V2 mx my) + globalOffset) env
transformGame (EventKey (Char 'q') Down _ (mx, my)) env = scaleKSep 1.3 env
transformGame (EventKey (Char 'a') Down _ (mx, my)) env = scaleKSep 0.7 env
transformGame (EventKey (Char 'w') Down _ (mx, my)) env = scaleKCoh 1.3 env
transformGame (EventKey (Char 's') Down _ (mx, my)) env = scaleKCoh 0.7 env
transformGame (EventKey (Char 'e') Down _ (mx, my)) env = scaleKAlign 1.3 env
transformGame (EventKey (Char 'd') Down _ (mx, my)) env = scaleKAlign 0.7 env
transformGame (EventKey (Char 'r') Down _ (mx, my)) env = scaleKAvoid 1.3 env
transformGame (EventKey (Char 'f') Down _ (mx, my)) env = scaleKAvoid 0.7 env
transformGame (EventKey (Char 'z') Down _ (mx, my)) env = killBirds env
transformGame (EventKey (Char 'x') Down _ (mx, my)) env = removeObsticles env
transformGame (EventKey (Char 'c') Down _ (mx, my)) env = removeShapes env
transformGame _ env = env



----------------------------

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
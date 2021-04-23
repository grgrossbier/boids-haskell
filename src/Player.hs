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


newEnviornment = 
    Enviornment 
    [ simpleCircle] 
    [ simpleTriangle] 
    centerOfMap
    (mkStdGen 10)

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
    addShape ((V2 mx my) + globalOffset) env
transformGame (EventKey (MouseButton RightButton) Down _ (mx, my)) env = 
    addObsticle ((V2 mx my) + globalOffset) env
transformGame _ env = env
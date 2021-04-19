module Player where

import qualified Graphics.Gloss as G
import Graphics.Gloss.Interface.Pure.Game

import Shapes
import Globals
import Rendering
import Physics
import Linear.V2


newEnviornment = 
    Enviornment 
    [ simpleCircle
    , centerCircle {sVelocity = V2 50 0}] 
    [centerCircle] 
    centerOfMap

window = G.InWindow "Functional" (screenWidth, screenHeight) (100, 100)
backgroundColor = G.makeColorI 0 0 0 255

playWithGraphics :: IO ()
playWithGraphics = 
    G.play 
        window 
        backgroundColor 
        frameRate
        newEnviornment 
        gameAsPicture 
        transformGame 
        advanceTime

transformGame :: Event -> p -> p
transformGame (EventKey (MouseButton LeftButton) Up _ mousePos) env = env
transformGame _ env = env
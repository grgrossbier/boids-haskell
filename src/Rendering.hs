module Rendering where

import qualified Graphics.Gloss as G
import Data.Array
import Linear.V2
import Text.Printf

import Shapes
import Globals
import ProjectMath

-- | Draw complete enviornment. 
enviornmentAsRunningPicture :: Enviornment -> G.Picture
enviornmentAsRunningPicture env =
    G.pictures  [ drawShapes env
                , G.color wallColor wallPicture
                , G.color obsticleColor $ obsticlesOfEnviornment env -- Only draws circles
                -- , G.color obsticleColor $ drawTriangles $ eObsticles env
                , drawBirds $ eBirds env
                , G.color failColor $ printStats env
                ]

printStats :: Enviornment -> G.Picture 
printStats env = G.pictures
    [ snapPictureToPosition (V2 xstart 10) kSep
    , snapPictureToPosition (V2 (xstart+1*xstep) 10) kCoh   
    , snapPictureToPosition (V2 (xstart+2*xstep) 10) kAlign 
    , snapPictureToPosition (V2 (xstart+3*xstep) 10) kAvoid
    ]
  where
    scale = 0.1
    xstart = 10
    xstep = 100
    kSep    = G.scale scale scale $ G.Text $ "kSep: " ++ kToString (eKSeparation env)
    kCoh    = G.scale scale scale $ G.Text $ "kCoh: " ++ kToString (eKCohesion env)
    kAlign  = G.scale scale scale $ G.Text $ "kAlign: " ++ kToString (eKAlignment env)
    kAvoid  = G.scale scale scale $ G.Text $ "kAvoid: " ++ kToString (eKAvoidance env)
    kToString k = printf "%.2f" k

snapPictureToPosition :: V2 Float -> G.Picture -> G.Picture
snapPictureToPosition (V2 x y) = G.translate x y

drawShapes :: Enviornment -> G.Picture
drawShapes env = 
    G.pictures  [ G.color ballColor $ drawCircles $ eShapes env
                , G.color triangleColor $ drawTriangles $ eShapes env
                ]

drawCircles :: [Shape] -> G.Picture 
drawCircles shapes = G.pictures $ map pictureCircle circles
  where
    circles = filter isCircle shapes
    getRadii shape
        | isCircle shape = (\(Circle r) -> r) (sGeometry shape)
    pictureCircle shape = 
        snapPictureToPosition (sPosition shape) . G.Circle $ getRadii shape

drawTriangles :: [Shape] -> G.Picture 
drawTriangles shapes = G.pictures $ map pictureCircle triangles
  where
    triangles = filter isTriangle shapes
    getHeight shape
        | isTriangle shape = (\(Triangle h _) -> h) (sGeometry shape)
    getBase shape
        | isTriangle shape = (\(Triangle _ b) -> b) (sGeometry shape)
    getAngle = sAngle
    getHeightBaseAngle s = (getHeight s, getBase s, getAngle s)
    pictureCircle shape = G.color (sColor shape)
        $ snapPictureToPosition (sPosition shape)
        $ uncurry3 drawTriangle (getHeightBaseAngle shape)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a,b,c) = f a b c 

drawTriangle :: Height -> Base -> Angle -> G.Picture
drawTriangle h b ang = 
    G.line
        [ v2ToTuple $ rotateV2 p1 ang
        , v2ToTuple $ rotateV2 p2 ang
        , v2ToTuple $ rotateV2 p3 ang
        , v2ToTuple $ rotateV2 p1 ang
        ]
  where
    v2ToTuple (V2 x y) = (x,y)
    p1 = V2 (-h/2) (b/2)
    p2 = V2 (h/2) 0
    p3 = V2 (-h/2) (-b/2)

wallPicture :: G.Picture 
wallPicture = 
    G.line 
        [ (1,1)
        , (1, fromIntegral screenHeight - 1)
        , (fromIntegral screenWidth - 1, fromIntegral screenHeight - 1)
        , (fromIntegral screenWidth - 1, 1)
        , (1,1)
        ]

obsticlesOfEnviornment :: Enviornment -> G.Picture 
obsticlesOfEnviornment env = G.pictures $ map pictureCircle circles
  where
    circles = filter isCircle $ eObsticles env
    getRadii shape
        | isCircle shape = (\(Circle r) -> r) (sGeometry shape)
    pictureCircle shape = 
        snapPictureToPosition (sPosition shape) . G.Circle $ getRadii shape

gameAsPicture :: Enviornment  -> G.Picture
gameAsPicture env = G.translate (fromIntegral screenWidth * (-0.5))
                                (fromIntegral screenHeight * (-0.5))
                                frame
    where frame = enviornmentAsRunningPicture env
                    -- GameOver winner -> boardAsGameOverPicture winner (gBoard game)

drawBirds :: [Shape] -> G.Picture 
drawBirds shapes = 
    G.pictures [drawTriangles shapes, drawCircles shapes]

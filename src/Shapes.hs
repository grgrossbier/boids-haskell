module Shapes where

import qualified Graphics.Gloss as G
import Linear.V2
import System.Random

import Globals

type Radius = Float
type Angle = Float
type Height = Float
type Base = Float
type Side = Float
data Enviornment = Enviornment
    { eBirds :: [Shape]
    , eShapes :: [Shape]
    , eObsticles :: [Shape]
    , eCenter :: V2 Float
    , eStdGen :: StdGen
    , eKSeparation :: Float
    , eKCohesion :: Float
    , eKAlignment :: Float
    , eKAvoidance :: Float
    } deriving (Show)

data Geometry = 
    Circle { getRadius :: Radius } 
    | Rectangle { getSide1 :: Side, getSide2 :: Side }
    | Triangle { getHeight :: Height, getBase :: Base }
    deriving (Eq, Show)

data Shape = Shape  
    { sGeometry :: Geometry
    , sAngle    :: Angle
    , sColor    :: G.Color
    , sPosition :: V2 Float
    , sVelocity :: V2 Float
    , sAcceleration :: V2 Float 
    , sLastPosition :: V2 Float} deriving (Show, Eq)

rotateShape :: Shape -> Angle -> Shape
rotateShape shape angle = shape { sAngle = sAngle shape + angle }

isCircle :: Shape -> Bool
isCircle s = case sGeometry s of Circle {} -> True
                                 _ -> False    

isRectangle :: Shape -> Bool
isRectangle s = case sGeometry s of Rectangle {} -> True
                                    _ -> False   

isTriangle :: Shape -> Bool
isTriangle s = case sGeometry s of  Triangle {} -> True
                                    _ -> False   

effectiveRadius :: Shape -> Float
effectiveRadius s
    | isCircle s = getRadius . sGeometry $ s
    | isTriangle s = max ((getHeight . sGeometry) s) ((getBase . sGeometry) s)/2
    | isRectangle s = max ((getSide1 . sGeometry) s) ((getSide2 . sGeometry) s)/1.7

simpleCircle :: Shape
simpleCircle = Shape 
    { sGeometry = Circle 25
    , sAngle    = 0
    , sColor    = ballColor
    , sPosition = centerOfMap
    , sVelocity = V2 (-70) 0
    , sAcceleration = V2 0 0 
    , sLastPosition = centerOfMap}

testShape :: Shape
testShape = simpleCircle { sPosition = V2 0 0
                         , sVelocity = V2 1 1 }

centerCircle :: Shape
centerCircle = Shape 
    { sGeometry = Circle 6
    , sAngle    = 0
    , sColor    = obsticleColor 
    , sPosition = centerOfMap
    , sVelocity = V2 0 0
    , sAcceleration = V2 0 0 
    , sLastPosition = centerOfMap}

simpleTriangle :: Shape
simpleTriangle = Shape 
    { sGeometry = Triangle 20 10
    , sAngle    = pi/4
    , sColor    = triangleColor  
    , sPosition = centerOfMap
    , sVelocity = V2 50 0
    , sAcceleration = V2 0 0 
    , sLastPosition = centerOfMap}

degToRadian :: Float -> Angle
degToRadian deg = deg*pi/180

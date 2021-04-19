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
    { eShapes :: [Shape]
    , eObsticles :: [Shape]
    , eCenter :: V2 Float
    , eStdGen :: StdGen
    } deriving (Show)


data Geometry = Circle Radius | Rectangle Side Side | Triangle Height Base
    deriving (Eq, Show)

data Shape = Shape  
    { sGeometry :: Geometry
    , sAngle    :: Angle
    , sColor    :: G.Color
    , sPosition :: V2 Float
    , sVelocity :: V2 Float
    , sAcceleration :: V2 Float 
    , sLastPosition :: V2 Float} deriving (Show, Eq)

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
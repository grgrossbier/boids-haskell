module Shapes where

import qualified Graphics.Gloss as G
import Globals

type Radius = Float
type Angle = Float
type Height = Float
type Base = Float
type Side = Float
data Enviornment = Enviornment
    { eShapes :: [Shape]
    , eObsticles :: [Shape]
    , eCenter :: (Float, Float)
    } deriving (Eq, Show)

data Geometry = Circle Radius | Rectangle Side Side | Triangle Height Base
    deriving (Eq, Show)

data Shape = Shape  
    { sGeometry :: Geometry
    , sAngle    :: Angle
    , sColor    :: G.Color
    , sPosition :: (Float, Float)
    , sVelocity :: (Float, Float)
    , sAcceleration :: (Float, Float) } deriving (Show, Eq)

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
    , sPosition = (300, 100)
    , sVelocity = (300,0)
    , sAcceleration = (0,0) }

centerCircle :: Shape
centerCircle = Shape 
    { sGeometry = Circle 10
    , sAngle    = 0
    , sColor    = ballColor
    , sPosition = centerOfMap
    , sVelocity = (0,0)
    , sAcceleration = (0,0) }
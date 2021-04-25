module Globals where

import Graphics.Gloss
import Linear.V2


ballColor :: Color
ballColor       = makeColorI 255 255 255 255
wallColor :: Color
wallColor       = makeColorI 255 50 50 255
obsticleColor :: Color
obsticleColor   = makeColorI 50 100 255 255
triangleColor :: Color
triangleColor   = makeColorI 50 255 50 255
failColor :: Color
failColor = greyN 1.0

screenWidth :: Int
screenWidth = 940

screenHeight :: Int
screenHeight = 780

frameRate :: Int
frameRate = 30

kSpring :: V2 Float -- Force / Pixel
kSpring = V2 3 3

gravity :: V2 Float
gravity = V2 0 (-400)

bounceEff :: Float
bounceEff = 0.98

dragCoeff :: Float
dragCoeff = 0.005 -- Force / v / Size

centerOfMap :: V2 Float
centerOfMap = V2 (fromIntegral screenWidth / 2) (fromIntegral screenHeight / 2)

globalOffset :: (Floating a, Num a) => V2 a
globalOffset = V2 (fromIntegral screenWidth / 2) (fromIntegral screenHeight / 2)

maxDist :: (Floating a, Num a) => a
maxDist = 100
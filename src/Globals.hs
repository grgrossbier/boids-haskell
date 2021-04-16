module Globals where

import Graphics.Gloss


ballColor :: Color
ballColor       = makeColorI 255 255 255 255
wallColor :: Color
wallColor       = makeColorI 255 50 50 255
obsticleColor :: Color
obsticleColor   = makeColorI 50 100 255 255
triangleColor :: Color
triangleColor   = makeColorI 50 255 50 255
failColor :: Color
failColor = greyN 0.5

screenWidth :: Int
screenWidth = 640

screenHeight :: Int
screenHeight = 480

frameRate :: Int
frameRate = 30

kSpring :: Float
kSpring = 5

centerOfMap :: (Float, Float)
centerOfMap = (fromIntegral screenHeight / 2, fromIntegral screenWidth / 2)
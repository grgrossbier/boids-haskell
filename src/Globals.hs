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
failColor = greyN 0.5

screenWidth :: Int
screenWidth = 640

screenHeight :: Int
screenHeight = 480

frameRate :: Int
frameRate = 30

kSpring :: V2 Float -- Force / Pixel
kSpring = V2 13 13

gravity :: V2 Float
gravity = V2 0 (-1000)

centerOfMap :: V2 Float
centerOfMap = V2 (fromIntegral screenWidth / 2) (fromIntegral screenHeight / 2)
module Rendering where

import qualified Graphics.Gloss as G
import Data.Array
import Linear.V2

import Shapes
import Globals
import ProjectMath

enviornmentAsRunningPicture :: Enviornment -> G.Picture
enviornmentAsRunningPicture env =
    G.pictures  [ drawShapes env
                , G.color wallColor wallPicture
                , G.color obsticleColor $ obsticlesOfEnviornment env
                , G.color obsticleColor $ drawTriangles $ eObsticles env
                ]

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
    pictureCircle shape = 
        snapPictureToPosition (sPosition shape)
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


-- snapPictureToCell :: (Integral a, Integral b) => Picture -> (a, b) -> Picture
-- snapPictureToCell picture (row, column) = translate x y picture
--     where x = fromIntegral column * cellWidth + cellWidth * 0.5
--           y = fromIntegral row * cellHeight + cellHeight * 0.5

-- xCell :: Picture
-- xCell = pictures [ rotate 45.0 $ rectangleSolid side 10.0
--                  , rotate (-45.0) $ rectangleSolid side 10.0
--                  ]
--     where side = min cellWidth cellHeight * 0.75

-- oCell :: Picture
-- oCell = thickCircle radius 10.0
--     where radius = min cellWidth cellHeight * 0.25

-- cellsOfBoard :: Board -> Cell -> Picture -> Picture
-- cellsOfBoard board cell cellPicture =
--     pictures
--     $ map (snapPictureToCell cellPicture . fst)
--     $ filter (\(_, e) -> e == cell)
--     $ assocs board

-- xCellsOfBoard :: Board -> Picture
-- xCellsOfBoard board = cellsOfBoard board (Just PlayerX) xCell

-- oCellsOfBoard :: Board -> Picture
-- oCellsOfBoard board = cellsOfBoard board (Just PlayerO) oCell

-- boardGrid :: Picture
-- boardGrid =
--     pictures
--     $ concatMap (\i -> [ line [ (i * cellWidth, 0.0)
--                               , (i * cellWidth, fromIntegral screenHeight)
--                               ]
--                        , line [ (0.0,                      i * cellHeight)
--                               , (fromIntegral screenWidth, i * cellHeight)
--                               ]
--                        ])
--       [0.0 .. fromIntegral n]

-- boardAsPicture board =
--     pictures [ xCellsOfBoard board
--              , oCellsOfBoard board
--              , boardGrid
--              ]

-- boardAsGameOverPicture winner board = color (outcomeColor winner) (boardAsPicture board)


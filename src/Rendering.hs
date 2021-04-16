module Rendering where

import qualified Graphics.Gloss as G
import Data.Array

import Shapes
import Globals

enviornmentAsRunningPicture :: Enviornment -> G.Picture
enviornmentAsRunningPicture env =
    G.pictures  [ G.color ballColor $ circlesOfEnviornment env
                , G.color wallColor wallPicture
                --  , color obsticleColor $ obsticlesOfEnviornment env
                --  , color triangleColor $ trianglesOfEnvironment env
                ]

snapPictureToPosition :: (Float, Float) -> G.Picture -> G.Picture
snapPictureToPosition (y, x) = G.translate x y

circlesOfEnviornment :: Enviornment -> G.Picture 
circlesOfEnviornment env = G.pictures $ map pictureCircle circles
  where
    circles = filter isCircle $ eShapes env
    getRadii shape
        | isCircle shape = (\(Circle r) -> r) (sGeometry shape)
    pictureCircle shape = 
        snapPictureToPosition (sPosition shape) . G.Circle $ getRadii shape
      

wallPicture :: G.Picture 
wallPicture = 
    G.line 
        [ (1,1)
        , (1, fromIntegral screenHeight - 1)
        , (fromIntegral screenWidth - 1, fromIntegral screenHeight - 1)
        , (fromIntegral screenWidth - 1, 1)
        , (1,1)
        ]

obsticlesOfEnviornment = undefined 
trianglesOfEnvironment = undefined 

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


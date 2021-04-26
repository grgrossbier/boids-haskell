module Main where

import Criterion
import Criterion.Main (defaultMain)
import System.Random
import Linear.V2

import Scratch
import Shapes
import Globals
import Game
import Player
import Text.Printf

main :: IO ()
main = do
    benches <- mapM (testBench 60) testQtys 
    defaultMain
        [ bgroup "BOIDS TEST" benches
        ]

testQtys :: [(Int, Int, Int)]
testQtys = (,,) <$> birds <*> shapes <*> obsticles
  where
    birds = [1,10,100,1000]
    shapes = [1]
    obsticles = [1,20]

testBench :: Int -> (Int,Int,Int) -> IO Benchmark 
testBench sec nums@(b,s,o)= do 
    gen <- mkStdGen <$> randomIO
    let env = createRandomEnv gen nums
    let label = printf "Test -- %d Birds -- %d Shapes -- %d Obsticles" b s o 
    return $ bench label $ whnf (runThenEvalLastFrame (sec*50)) env

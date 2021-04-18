module ProjectMath where

import Linear.V2
import Linear.Metric

scale :: Num a => a -> V2 a -> V2 a
scale a v = (a*) <$> v
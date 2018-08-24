module Hypercube where

import Vector

data Hypercube = Hypercube { minV :: Vector
                           , maxV :: Vector
                           } deriving (Show, Eq)

fromRadius :: Vector -> Double -> Hypercube
fromRadius center rad = Hypercube lower upper
    where rad'  = rad / 2
          lower = (-rad'+) <$> center
          upper = ( rad'+) <$> center

inside :: Vector -> Hypercube -> Bool
[] `inside` Hypercube [] [] = True
pt `inside` t = p > x && p < y && ps `inside` rest
    where p:ps = pt
          x:xs = minV t
          y:ys = maxV t
          rest = Hypercube xs ys

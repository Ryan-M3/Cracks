module Edge where

import Vector

data Edge = Edge { from :: Vector
                 , to   :: Vector
                 , leaf :: Bool
                 } deriving (Eq, Show)

edgeLen :: Edge -> Double
edgeLen edge = let v = from edge `less` to edge
                in magnitude v

toVec :: Edge -> Vector
toVec edge = (to edge) `less` (from edge)

edgeAngle :: Edge -> Edge -> Double
edgeAngle edge1 edge2 = angle (toVec edge1) (toVec edge2)

module Vector where

import GHC.Float
import Debug.Trace

type Vector = [Double]

add :: Vector -> Vector -> Vector
v `add` u = zipWith (+) v u

less :: Vector -> Vector -> Vector
v `less` u = zipWith (-) v u

times :: Vector -> Double -> Vector
v `times` x = map (x*) v

divide :: Vector -> Double -> Vector
v `divide` x = map (/x) v

dist :: Vector -> Vector -> Double
dist v u = sqrt (sum $ (**2) <$> (v `less` u))

dot :: Vector -> Vector -> Double
v `dot` u = sum $ zipWith (*) v u

angle :: Vector -> Vector -> Double
angle v u = let theta = (v `dot` u) / (magnitude v + magnitude u)
             in acos theta

normal :: Vector -> Vector
normal v = v `divide` (magnitude v)

clamp :: Vector -> Double -> Vector
clamp v len
  | magnitude v == 0  = error "Cannot clamp zero vector."
  | magnitude v < len = v
  | otherwise = normal v `times` len

magnitude :: Vector -> Double
magnitude v = dist (repeat 0) v

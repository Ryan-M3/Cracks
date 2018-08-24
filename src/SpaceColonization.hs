module SpaceColonization where

import Data.List (nub)
import System.Random

import Vector
import Hypercube
import Edge
import KDTree

-- If you don't weight by distance, the results are absolute garbage;
-- for some reason just assuming that the nearest 5 are all about
-- equidistant just doesn't work at all.
netPullDir :: Vector -> [Vector] -> Vector
netPullDir center [] = center `times` 0
netPullDir center nhood = sum `divide` n
    where diffs = wtFn <$> nhood
          sum   = foldl add (repeat 0) diffs
          n     = fromIntegral (length nhood)
          wtFn  = \v -> (v `less` center) `divide` ((dist center v)**2)

grow :: Vector -> Tree -> Double -> Double -> Vector
grow v tree searchRad growAmt = v `add` (net `times` growAmt)
    where area = fromRadius v searchRad
          net  = netPullDir v $ findIn tree area

kill :: Vector -> Tree -> Double -> Tree
kill v tree killDist =  let area = fromRadius v killDist
                         in foldl del tree (findIn tree area)

rndVs :: Int -> Double -> [Vector]
rndVs seed scale = zipWith (\x y -> [x, y]) rnds1 rnds2
    where mkrnd s = nub $ (*scale) <$> randoms (mkStdGen s) :: [Double]
          rnds1   = mkrnd seed
          rnds2   = mkrnd (round $ abs(rnds1!!0))

rndTree :: Int -> Int -> Double -> Tree
rndTree seed density scale = foldl insert root (take density $ rndVs seed scale)

getEdgePt :: [Edge] -> Int -> Edge
getEdgePt edges rnd = let e = edges!!(abs(rnd) `rem` length edges)
                       in e

mkCrack :: [Int]  -- list of random numbers
        -> Tree   -- (initial) tree of auxins / random points
        -> [Edge] -- starting edge or intermediate list of edges
        -> Double -- how big of an area to look for auxins
        -> Double -- how far to grow each iteration
        -> Double -- how close an auxin needs to be to a vein to be removed
        -> [Edge] -- crack represented as a list of edges
mkCrack (r:rs) tree edges searchRad growDist killDist
  | isEmpty tree   = edges
  | length rs == 0 = edges
  | otherwise      = mkCrack rs tree' edges' searchRad growDist killDist
  where rndE   = getEdgePt edges r
        v      = to rndE
        v'     = grow v tree searchRad growDist
        tree'  = kill v' tree killDist
        edge   = Edge v v' True
        acute  = let a = (57.2958 * edgeAngle rndE edge) `castOut` 180
                  in a < 30 || a > 150
        edges' = if ((edgeLen edge) < 0.01 && (leaf rndE)) || acute && (leaf rndE)
                 then edges
                 else nub $ edge:(unsetLeaf edges rndE)

-- "cast out" is a term I heard in abook from 1960 about arithmetic
-- for adults who have forgotten most of it from school. The phrase
-- "the method of cast out nines" seems to still be used today. This
-- differs from `rem` and `rem` in that it's defined for zero.
castOut :: Double -> Double -> Double
num `castOut` divisor
  | num == 0      = num
  | num <  0      = -((-num) `castOut` divisor)
  | num < divisor = num
  | otherwise     = (num - divisor) `castOut` divisor

unsetLeaf :: [Edge] -> Edge -> [Edge]
unsetLeaf [] edge = []
unsetLeaf (e:es) edge
  | e == edge = (Edge (from edge) (to edge) False):es
  | otherwise = e : (unsetLeaf es edge)

mkCrack' :: Int    -- seed
         -> Int    -- density of auxins
         -> Double -- scale
         -> Double -- searchRad
         -> Double -- growDist
         -> Double -- distance from vein/crack at which auxins are removed
         -> Int    -- maxIters (more or less size of crack)
         -> [Edge] -- crack
mkCrack' seed density scale searchRad growDist killDist maxIters =
    mkCrack rnds tree edges searchRad growDist killDist
    where rnds  = take maxIters $ randoms (mkStdGen seed) :: [Int]
          edges = [Edge [0, 0] [0, 0] True]
          seed2 = fromIntegral (rnds!!0) :: Int
          tree  = rndTree (abs seed2) density scale

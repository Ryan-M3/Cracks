module Main where

import Graphics.Gloss
import Debug.Trace
import Data.List (nub)
import System.Random
import Lib

-- Simulation Parameters
-- Honestly, the quality of the results seems
-- to be hypersensitive to the parameters.
killDist  = 1.5  :: Double
growDist  = 5    :: Double
searchRad = 30   :: Double
treeScale = 200  :: Double  -- name "scale" hides Gloss.scale
density   = 500  :: Int
maxIters  = 3000 :: Int

window :: Display
window = InWindow "Space Colonization Algorithm" (500, 500) (10, 10)

background :: Color
background = white

edgesToPaths :: [Edge] -> [Path]
edgesToPaths [] = []
edgesToPaths (e:es) = [(a, b), (c, d)]:(edgesToPaths es)
    where [a, b] = realToFrac <$> from e
          [c, d] = realToFrac <$> to   e

getDrawing :: Int -> Picture
getDrawing seed = let crack = mkCrack' seed density treeScale searchRad growDist killDist maxIters
                      paths = edgesToPaths $ crack
                   in Scale 25 25 $ Pictures $ Line <$> paths

main :: IO ()
main = do
    seed <- randomIO 
    display window background (getDrawing seed)

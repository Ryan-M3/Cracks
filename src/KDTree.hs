module KDTree where

import Numeric
import Data.List (intercalate, sortBy)
import Debug.Trace

import Vector
import Hypercube

data Tree = Empty { depth :: Int }
          | Node  { depth :: Int
                  , key   :: Vector
                  , left  :: Tree
                  , right :: Tree
                  } deriving (Eq)

instance Show (Tree) where
    show = showLvl 0
        where
            showLvl _ Empty {} = ""
            showLvl n node =
                let l = left node
                    r = right node
                    m = n + 1
                    strFs   = (\x -> showFFloat (Just 2) x "") <$> key node
                    thisStr =  "Node(" ++ intercalate ", " strFs ++ ")"
                    indent  = "\n" ++ (concat $ replicate n "  ")
                 in indent ++ thisStr ++ showLvl m l ++ showLvl m r

root :: Tree
root = Empty {depth=0}

isEmpty :: Tree -> Bool
isEmpty Empty {} = True
isEmpty _        = False

isLeaf :: Tree -> Bool
isLeaf node = isEmpty (left node) && isEmpty (right node)

insert :: Tree -> Vector -> Tree
insert Empty {depth=d} v = Node { depth = d
                                , key   = v
                                , left  = Empty (d + 1)
                                , right = Empty (d + 1)
                                }
insert node v = if v `isLeftOf` node
                then node { left  = insert (left  node) v }
                else node { right = insert (right node) v }

dim :: Tree -> Int
dim node = depth node `rem` k
    where k = length $ key node 


listTree :: Tree -> [Tree]
listTree Empty{} = []
listTree tree    = [tree] ++ listTree (left tree) ++ listTree (right tree)

count :: Tree -> Int
count Empty{} = 0
count tree    = 1 + count (left tree) + count (right tree)

isLeftOf :: Vector -> Tree -> Bool
query `isLeftOf` node = query!!k < (key node)!!k
    where k = dim node

subnode node v = if v `isLeftOf` node
                 then left node
                 else right node

has :: Tree -> Vector -> Bool
Empty {} `has` v = False
node     `has` v = key node == v || (subnode node v) `has` v

minNode :: Int -> Tree -> Tree -> Tree
minNode k Empty{} second = second
minNode k first Empty{}  = first
minNode k first second   = if (key first)!!k < (key second)!!k
                           then first else second

findMin :: Int -> Tree -> Tree
findMin k node
  | isEmpty node   = node
  | dim node == k  = minNode k node minL
  | otherwise      = minNode k node $ minNode k minL minR
    where minL = findMin k (left  node)
          minR = findMin k (right node)

del :: Tree -> Vector -> Tree
del node v
  | isEmpty node               = node
  | isMatch && isLeaf node     = Empty {depth = depth node} 
  | isMatch && not (isEmpty r) = node { key = rKeyMin, right = del r rKeyMin }
  | isMatch && not (isEmpty l) = node { key = lKeyMin, left  = del l lKeyMin }
  | v `isLeftOf` node          = node { left = del l lKeyMin }
  | otherwise                  = node { right = del r rKeyMin }
    where isMatch = key node == v
          r       = right node
          l       = left  node
          k       = dim node
          rKeyMin = key $ findMin k r
          lKeyMin = key $ findMin k l

findIn :: Tree -> Hypercube -> [Vector]
findIn Empty{} area = []
findIn node    area = found ++ foundL ++ foundR
    where k = dim node
          foundL = if (key node)!!k > (minV area)!!k
                   then findIn (left node) area
                   else []
          foundR = if (key node)!!k < (maxV area)!!k
                   then findIn (right node) area
                   else []
          found  = if (key node) `inside` area
                   then [key node]
                   else []

nearest :: Int -> Tree -> Vector -> Double -> [Vector]
nearest n tree v r
  | r > 100          = take n $ sortBy distFn (key <$> listTree tree)
  | count tree   < n = key <$> listTree tree
  | length found < n = nearest n tree v (r + 1)
  | length found > n = take n $ sortBy distFn found
  | otherwise        = found
    where found  = findIn tree $ fromRadius v r
          distFn = \x y -> compare (dist v x) (dist v y)

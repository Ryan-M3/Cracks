module TestKDTree (treeTestSuite, generator, nodeA) where

import Test.HUnit
import KDTree
import Vector
import Hypercube

testInsertOne :: Assertion
testInsertOne = assertBool "Unit test for add failed." (a == b)
    where a = insert root [1, 1]
          b = Node { depth = 0, key = [1, 1], left = Empty 1, right = Empty 1}

testHas :: Assertion
testHas = assertBool "Unit test for has failed." $ all (nodeA `has`) generator

testInsert :: Assertion
testInsert = assertBool str (made == nodeA)
    where made = foldl insert root generator 
          str = "Unit test failed: " ++ (show made)

testFindMin :: Assertion
testFindMin = key (findMin 1 nodeA) @?= (key nodeE)

testDel :: Assertion
testDel = count <$> (scanl del nodeA generator) @?= reverse [0..5]

testFindIn :: Assertion
testFindIn = findIn nodeA area @?= [[3, 3]]
    where area = Hypercube [2.5, 2.5] [3.5, 3.5]

-- Adding these points (generator) to an empty node from left
-- to right should result in a tree structure described below.
generator = [[1, 1], [2, 2], [3, 3], [1, 2], [4, -3]]

{- TREE STRUCTURE
   a l: None
     r: b    l: e   l: None
                    r: None
             r: c   l: d    l: None
                            r: None
                    r: None

   SIMPLIFIED REPRESENTATION
    a
      b
        e
        c
          d
-}

nodeA = Node { depth = 0
             , key   = [1, 1]
             , left  = Empty 1
             , right = nodeB
             }

nodeB = Node { depth = 1 
             , key   = [2, 2]
             , left  = nodeE
             , right = nodeC
             }

nodeC = Node { depth = 2
             , key   = [3, 3]
             , left  = nodeD
             , right = Empty 3
             }

nodeD = Node { depth = 3
             , key   = [1, 2]
             , left  = Empty 4
             , right = Empty 4
             }

nodeE = Node { depth = 2
             , key   = [4, -3]
             , left  = Empty 3
             , right = Empty 3
             }

treeTestSuite :: IO ()
treeTestSuite = do
    testInsertOne
    testInsert
    testHas
    testFindMin
    testDel
    testFindIn

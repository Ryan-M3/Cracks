module TestSpaceColonization (spaceTestSuite) where

import Test.HUnit
import TestKDTree
import KDTree
import SpaceColonization
import Vector

simplifyFloat :: Double -> Int
simplifyFloat f = round (f * 10.0)

testNetPullDir :: Assertion
testNetPullDir = gotBack @?= precalc
    where center = [3, 2]
          nhood  = generator -- imported from TestKDTree
          gotBack = simplifyFloat <$> netPullDir center nhood 
          precalc = [-4, 1]

testGrow :: Assertion
testGrow = grown @?= precalc
    where grown   = simplifyFloat <$> grow [3, 2] nodeA 100 2
          precalc = [23, 22]

testKill :: Assertion
testKill = key (kill [3, 2] nodeA 10) @?= [4, -3]

spaceTestSuite :: IO ()
spaceTestSuite = do
    testNetPullDir
    testGrow
    testKill

module TestVector (vectorTestSuite) where

import Debug.Trace
import Test.HUnit

import Vector

testAdd :: Assertion
testAdd = [0, 1, 2, 3] `add`  [2, 3, 3, 1] @?= [2, 4, 5, 4]

testSub :: Assertion
testSub = [0, 1, 2, 3] `less` [2, 3, 3, 1] @?= [-2, -2, -1, 2]

testTimes :: Assertion
testTimes = [0, 1, 2, 3] `times` 3 @?= [0, 3, 6, 9]

testDivide :: Assertion
testDivide = [12, 12] `divide` 3 @?= [4, 4]

testDist :: Assertion
testDist = assertBool msg test
    where msg  = "testDist failed, got value: " ++ (show d)
          test = d < 4.25 && d > 4.24
          d    = dist [1, 2] [4, 5]

testMagnitude :: Assertion
testMagnitude = assertBool msg test
    where msg  = "testMagnitude failed, got value: " ++ (show m)
          test = m < 7.35 && m > 7.34
          m    = magnitude [2, 3, 4, 5] 

testAngle :: Assertion
testAngle = round (theta * 100)  @?= 157
    where theta = angle [1, 0] [0, 1]

testNormal :: Assertion
testNormal = 1 @?= (magnitude $ normal [3, 2])

testClamp :: Assertion
testClamp = normal [99, 99] @?= clamp [99, 99] 1

vectorTestSuite :: IO ()
vectorTestSuite = do
    testAdd
    testSub
    testTimes
    testDivide
    testDist
    testMagnitude
    testAngle
    testNormal
    testClamp

module Lab3 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

testTaut = Dsj [Prop 1, (Neg (Prop 1))]
testContra = Cnj [Prop 1, (Neg (Prop 1))]
testEntails1 = Prop 1
testEntails2 = Neg (Prop 1)

-- Exercise 1
contradiction, tautology :: Form -> Bool
tautology f = all (\ v -> evl v f) (allVals f)
contradiction f = not (satisfiable f)

entails :: Form -> Form -> Bool
entails f1 f2 = all (\ v -> (evl v f1) --> (evl v f2)) (allVals (Impl f1 f2))

equivalence :: Form -> Form -> Bool
equivalence f1 f2 = (entails f1 f2) && (entails f2 f1)

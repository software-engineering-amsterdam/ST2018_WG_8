module Lab3 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3


---------------------------1
--always true
tautology :: Form -> Bool
tautology f = all (\ v -> evl v f) (allVals f)

--always false, so it cannot be satisfiable
contradiction :: Form -> Bool
contradiction f = not(satisfiable f)

entails :: Form-> Form -> Bool
entails f g = tautology (Impl f g)

equiv :: Form -> Form -> Bool
equiv f g = tautology (Equiv f g)

----------------------------3
--the first step is to remove arrows and negations. Both of these function are given 
--in Lecture3.hs file
firstStep :: Form -> Form
firstStep f = (nnf . arrowfree) f


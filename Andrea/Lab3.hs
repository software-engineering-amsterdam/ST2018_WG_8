module Lab3 where
 
import Data.List
import Lecture3
import System.Random
import Test.QuickCheck
    

{- 
    Exercise 1: Give definitions.
    Deliverables: implementation, description of your method
    of checking the definitions, indication of time spent.
-}

-- tautology: All statements satisfy.
tautology :: Form -> Bool
tautology f = all (\v -> evl v f) (allVals f)

-- contradiction: No statements satisfy.
contradiction :: Form -> Bool
contradiction f = not (satisfiable f)

-- entails: B logically entails A is true if and only if
-- all of the truth valuations that satisfy B also satisfy A.
entails :: Form -> Form -> Bool
entails f g = all (\v -> evl v f --> evl v g) (allVals (Impl f g))

-- equiv: A & B are equivalent.
equiv :: Form -> Form -> Bool
equiv f g = entails f g && entails g f

cnjP, cnjQ, dsjP, dsjQ :: Form
cnjP = Cnj [p, Neg p]
cnjQ = Cnj [q, Neg q]
dsjP = Dsj [p, Neg p]
dsjQ = Dsj [q, Neg q]

main = do 
    putStrLn("Testing exercise 1. These should return TRUE:")
    -- INSERT TESTS HERE
    putStrLn("Testing exercise 1. These should return FALSE:")
    -- INSERT TESTS HERE.

{- 
    Exercise 2: Test the parsing function.
    Deliverables: test report describing the test method used
    and the outcome of the test, indication of time spent.
-}
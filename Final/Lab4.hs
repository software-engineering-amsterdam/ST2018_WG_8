module Lab4 where

import Control.Monad
import Data.List
import Data.Char
import SetOrd
import System.Process
import System.Random
import Test.QuickCheck


{-
    Exercise 1: Read or reread Chapter 4.
    (Deliverables: list of questions, indication of time spent.)

    - Andrea found the Russell Paradox  difficult. 
-}

{-
    Exercise 2: Implement a random data generator for the datatype Set Int.
    First do this from scratch, then with QuickCheck to random test this datatype.
    (Deliverables: two random test generators, indication of time spent.)
-}

{-
    Exercise 3: Implement operations for set intersection, set union and set difference.
    Next, use automated testing to check that your implementation is correct.
    First use your own generator, next use QuickCheck.
    (Deliverables: implementations, test properties, short test report,
    indication of time spent.)
-}

{-
    Exercise 4: Read or reread Chapter 5.
    (Deliverables: list of questions, indication of time spent.)
-}

{-
    Exercise 5: Implement a function symClos. 
    (Deliverable: Haskell program, indication of time spent.)
-}

symClos :: Ord a => Rel a -> Rel a

{-
    Exercise 6: Define a function trClos.
    (Deliverable: Haskell program, indication of time spent.)
-}

trClos :: Ord a => Rel a -> Rel a

{-
    Exercise 7: Test the functions symClos and trClos from the previous exercises.
    Devise your own test method for this. Try to use random test generation.
    Define reasonable properties to test. Can you use QuickCheck? How?
    (Deliverables: test code, short test report, indication of time spent.)
-}

{-
    Exercise 8: Is there a difference between the symmetric closure of the transitive
    closure of a relation R and the transitive closure of the symmetric
    closure of R?
    
    (Deliverable: If your answer is that these are the same, you should give an
    argument, if you think these are different you should give an example that
    illustrates the difference.)
-}
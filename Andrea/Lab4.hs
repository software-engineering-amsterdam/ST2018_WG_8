module Lab4 where

import Control.Monad
import Data.List
import Data.Char
import Lecture4
import SetOrd
import System.Process
import System.Random
import Test.QuickCheck


{-
    Exercise 1:
    Read or reread Chapter 4 of The Haskell Road, and make a list of questions
    on specific points that cause difficulty of understanding.
    (Deliverables: list of questions, indication of time spent.)
-}

-- instance (Arbitrary Set) Int where
-- arbitrary = 


{-
    Exercise 2:
    Implement a random data generator for the datatype Set Int, where Set is as
    defined in SetOrd.hs. First do this from scratch, next give a version that
    uses QuickCheck to random test this datatype.
    (Deliverables: two random test generators, indication of time spent.)
-}

{-
    Exercise 3:
    Implement operations for set intersection, set union and set difference,
    for the datatype Set defined in SetOrd.hs. Next, use automated testing
    to check that your implementation is correct. First use your own generator,
    next use QuickCheck.
    (Deliverables: implementations, test properties, short test report,
    indication of time spent.)
-}

{-
    Exercise 4:
    Read or reread Chapter 5 of The Haskell Road, and make a list of questions
    on specific points that cause difficulty of understanding.
    (Deliverables: list of questions, indication of time spent.)
-}

{-
    Exercise 5: Implement a function that gives the symmetric closure of a relation,
    where the relation is represented as an ordered list of pairs.
    E.g., symClos [(1,2),(2,3),(3,4)] should give [(1,2),(2,1),(2,3),(3,2),(3,4),(4,3)].
    Time spent: +- 10 minutes.
-}

type Rel a = [(a,a)]

-- Remove all duplicates & sort the tuples of the relation.
symClos :: Ord a => Rel a -> Rel a
symClos xs = sort( nub( addSym xs))

-- Adds all symmetrical counterparts of the tuples in the relation.
addSym :: Ord a => Rel a -> Rel a
addSym  [] = []
addSym ((a,b) : xs) = (a, b) : (b, a) : addSym xs

{-
    Exercise 6: Define a function that gives the transitive closure of a relation,
    represented as an ordered list of pairs.
    E.g., trClos [(1,2),(2,3),(3,4)] should give [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)].
    (Deliverable: Haskell program, indication of time spent.)
    -- Time spent: WAY LONGER THAN I SHOULD HAVE (1 Hr.)
-}

infixr 5 @@

-- Given functione.
(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

-- Create new transitions of the relation until no new relations are found.
trClos :: Ord a => Rel a -> Rel a
trClos xs = fp addTrans xs

-- Create the next transition of the current relation.
addTrans :: Ord a => Rel a -> Rel a
addTrans xs = sort( nub( xs ++ (xs @@ xs)))

{-
    Exercise 7: Test the functions symClos and trClos from the previous exercises.
    Devise your own test method for this. Try to use random test generation.
    Define reasonable properties to test. Can you use QuickCheck? How?
    (Deliverables: test code, short test report, indication of time spent.)
-}

{-
    Exercise 8:
    Is there a difference between the symmetric closure of the transitive
    closure of a relation R and the transitive closure of the symmetric
    closure of R?
    (Deliverable: If your answer is that these are the same, you should give an
    argument, if you think these are different you should give an example that
    illustrates the difference.)
-}

{-
    Bonus:
    In the lecture notes, Statement is in class Show, but the show function
    for it is a bit clumsy. Write your own show function for imperative programs.
    Next, write a read function, and use show and read to state some abstract
    test properties for how these functions should behave. Next, use QuickCheck
    to test your implementations.
    (Deliverable: implementation, QuickCheck properties, test report.)
-}

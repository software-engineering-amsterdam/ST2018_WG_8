module Lab4 where

import Control.Monad
import Data.Char
import Data.List
import Data.Tuple
import Lecture4
import SetOrd
import System.Process
import System.Random
import Test.QuickCheck


{-
    Exercise 1: Read or reread Chapter 4 of The Haskell Road, and make a list of questions
    on specific points that cause difficulty of understanding.
    (Deliverables: list of questions, indication of time spent.)
    
    Russel Paradox (p. 131)
    +- 1 hour.
-}

{-

    TO DO!! 
    Exercise 2:
    Implement a random data generator for the datatype Set Int, where Set is as
    defined in SetOrd.hs. First do this from scratch, next give a version that
    uses QuickCheck to random test this datatype.
    (Deliverables: two random test generators, indication of time spent.)
-}

instance Arbitrary (Set a)  where
    arbitrary = do
        set <- arbitrary
        return Set (nub (sort ( set)))

{-
    Exercise 3: Implement operations for set intersection, set union and set difference.
    Next, use automated testing to check that your implementation is correct.
    First use your own generator, next use QuickCheck.
    (Deliverables: implementations, test properties, short test report,
    indication of time spent.)
-}

-- intersection of a set : doorsnede, what is in both sets?
setIntersection :: Ord a => Set a -> Set a -> Set a
-- setIntersection (Set [])     set2  = []
setIntersection (Set (xs)) set2  = Set ([x | x <- xs, inSet x set2])

-- union of a set : vereniging, the total of both sets?
setUnion :: Ord a => Set a -> Set a -> Set a
setUnion a b = unionSet a b 

-- difference of a set: A - B
-- The \\ function is list difference ((non-associative). In the result of xs \\ ys, the first occurrence of each 
-- element of ys in turn (if any) has been removed from xs
setDifference :: Ord a => Set a -> Set a -> Set a
setDifference (Set [])     set2  =  set2
setDifference (Set (xs)) (Set (ys))  = Set (sort (xs \\ ys) ++ (ys \\ xs))

-- CREATE TESTS!! Or just use Rens' :)

{-
    Exercise 4: Read or reread Chapter 5 of The Haskell Road, and make a list of questions
    on specific points that cause difficulty of understanding.
    (Deliverables: list of questions, indication of time spent.)
    - reflexive transitive closure
    Time spent: +- 40 minutes.

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
addSym (x : xs) = x : swap x : addSym xs

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

checkSym :: Eq a => Rel a -> Bool
checkSym [] = True
checkSym (x : xs) = 
    if (swap x `elem` (x : xs)) == False
    then False
    else checkSym(delete (swap x) xs)  

-- Quickcheck: Create list of tuples of Relations

-- Given a Relation xRy, create a list of all relations in that list that correspond with yRz.
createPairs :: Eq a => (a, a) -> Rel a -> Rel a
createPairs xRy xs = filter (\(y,_) -> y == snd xRy) xs

checkTrans :: Eq a => Rel a -> Bool
checkTrans xs = all (== True) [ (x,z) `elem` xs | (x,y) <- xs, (_,z) <- (createPairs (x,y) xs)]
-- STILL NEEDS TO CHECK IF THERE ARE ELEMS IN TRANS THAT SHOULD NOT BE THERE 

{-
    TO DO!! 

    Exercise 8:
    Is there a difference between the symmetric closure of the transitive
    closure of a relation R and the transitive closure of the symmetric
    closure of R?
    (Deliverable: If your answer is that these are the same, you should give an
    argument, if you think these are different you should give an example that
    illustrates the difference.)

    Yes they are the same. They are associative. 
    https://proofwiki.org/wiki/Transitive_Closure_of_Symmetric_Relation_is_Symmetric
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

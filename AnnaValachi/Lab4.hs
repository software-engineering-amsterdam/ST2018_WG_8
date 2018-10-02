module Lab4 where

import Control.Monad
import Data.List
import Data.Char
import SetOrd
import System.Process
import System.Random
import Test.QuickCheck

-- Generates a random int.
randomNumber :: IO Int
randomNumber = randomRIO (0,100)

-- Generates a random list of ints within a range [-n..n] and of x length.
randomNumberList :: Int -> Int -> IO [Int]
randomNumberList n x = replicateM x $ randomRIO (-n,n)

setLength (Set xs) = length xs
{-
    Exercise 1:
    Read or reread Chapter 4 of The Haskell Road, and make a list of questions
    on specific points that cause difficulty of understanding.
    (Deliverables: list of questions, indication of time spent.)
-}
--ex 4.25

{-
    Exercise 2:
    Implement a random data generator for the datatype Set Int, where Set is as
    defined in SetOrd.hs. First do this from scratch, next give a version that
    uses QuickCheck to random test this datatype.
    (Deliverables: two random test generators, indication of time spent.)
-}

-- This generator from scratch gets 2 numbers as range and size of the set,
-- then it generates a list and feeds it to the Set generator.
scratchGen :: IO (Set Int)
scratchGen = do
    range <- randomNumber
    size <- randomNumber
    list <- randomNumberList range size
    let list' = nub list
    return (Set (sort list'))

-- The second generator is really simple but took me longer as the notation
-- was new to me, a generator for arbitrary is defined to generate lists.
setGen = arbitrary :: Gen ([Int])

generateSet :: Gen (Set Int)
generateSet = do
    list <- setGen
    let list' = nub list
    return (Set (sort list'))

-- Use an instance to create a set to be used in QuickCheck.
instance (Arbitrary a, Ord a) => Arbitrary (Set a) where
    arbitrary = do
        t <- arbitrary
        let t' = sort(nub(t))
        return (Set t)

-- Before either lists are turned to sets, duplicates are removed and the list is
-- sorted.


{-
    Exercise 3:
    Implement operations for set intersection, set union and set difference,
    for the datatype Set defined in SetOrd.hs. Next, use automated testing
    to check that your implementation is correct. First use your own generator,
    next use QuickCheck.
    (Deliverables: implementations, test properties, short test report,
    indication of time spent.)
-}


intersection ::Ord a=> Set a -> Set a -> Set a
intersection (Set a) (Set b) =  Set ([x|x<-a, inSet x (Set b)])

myunion :: Ord a => Set a -> Set a -> Set a
myunion (Set []) (Set b) = Set b
myunion (Set a) (Set b) = Set (nub (sort (a ++ b)))

difference :: Eq a=> Set a -> Set a -> Set a
difference (Set a) (Set b) = Set ([x|x<-a, not(x `elem` b) ])

--myIntersectionTest :: Eq a=> Set a -> Set a -> Bool
--myIntersectionTest a b =  length ([x|x<- intersection (Set a) (Set b), not(x `elem` a)||not(x `elem` b)])==0

--myTestDidderence :: Eq a=> Set a -> Set a -> Bool
--myTestDidderence a b = length [x|x <- diff , inSet x a, not(inSet x b)]==0
    --where diff = difference a b

-- Intersection tests that each element of the list is in both result lists.
-- In other words the intersection should be a subset of both lists.
-- Furthermore, the function should not remove excess items, therefore the same
-- list should not loose any items.
{-testIntersectionSet :: Ord a => Set a -> Set a -> Bool
testIntersectionSet set1 set2 =
    if set1 == set2
        then setLength (intersectionSet set1 set2) == setLength set1
        else crossCheckEls set1 set2

crossCheckEls :: Ord a => Set a -> Set a -> Bool
crossCheckEls set1 set2 = subSet setIntersection set1 && subSet setIntersection set2
    where
        setIntersection = intersectionSet set1 set2-}


{-
    Exercise 4:
    Read or reread Chapter 5 of The Haskell Road, and make a list of questions
    on specific points that cause difficulty of understanding.
    (Deliverables: list of questions, indication of time spent.)
-}

{-
    Exercise 5:
    Suppose we implement binary relations as list of pairs, Haskell type [(a,a)].
    Assume the following definition:

    > type Rel a = [(a,a)]

    Use this to implement a function

    symClos :: Ord a => Rel a -> Rel a

    that gives the symmetric closure of a relation, where the relation is
    represented as an ordered list of pairs. E.g., symClos [(1,2),(2,3),(3,4)]
    should give [(1,2),(2,1),(2,3),(3,2),(3,4),(4,3)].
    (Deliverable: Haskell program, indication of time spent.)
-}

--Time : 10 minutes
--if the relation is not ordered we need to order it/ i couldnt understand if it is or not!! Ask

type Rel a = [(a,a)]

sym :: Ord a => Rel a->Rel a
sym [] = []
sym (x:xs) = x:(snd x, fst x):sym xs

symClos :: Ord a =>Rel a->Rel a
symClos [] = []
symClos xs = sort (nub (sym ( xs)))

{-
    Exercise 6:
    Use the datatype for relations from the previous exercise, plus

    > infixr 5 @@
    >
    > (@@) :: Eq a => Rel a -> Rel a -> Rel a
    > r @@ s =
    >   nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

    to define a function

    trClos :: Ord a => Rel a -> Rel a

    that gives the transitive closure of a relation, represented as an ordered
    list of pairs. E.g., trClos [(1,2),(2,3),(3,4)] should give
    [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)].
    (Deliverable: Haskell program, indication of time spent.)
-}


infixr 5 @@
(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

--rel @@ rel gives us the new tuples that we need to add to our relation
--unionRel creates a new relation which is the union of our first relation and the tuples that we need to add to it
unionRel rel = nub (rel ++ (rel @@ rel)) 


fp :: Eq a => (a -> a) -> a -> a
fp f = until (\ x -> x == f x) f

--when we cannot add more tuples to our list we should stop.
--This means that the input relation == output relation
--so this is our stop condition. We used the function fp from the lecture.
trClos ::Ord a=>Eq a => Rel a -> Rel a
trClos [] = []
trClos rel = sort (fp unionRel rel)



{-
    Exercise 7:
    Test the functions symClos and trClos from the previous exercises.
    Devise your own test method for this. Try to use random test generation.
    Define reasonable properties to test. Can you use QuickCheck? How?
    (Deliverables: test code, short test report, indication of time spent.)
-}
--SymTest
testSym :: Eq a=>Rel a -> Bool
testSym [] = True
testSym xs = length([a|a<- xs, not ((snd a,fst a) `elem` xs)]) == 0

--Tr Test
createList ::Eq a=> Rel a -> Rel a
createList [] = []
createList (x:xs)= [a | a<-xs, (snd x)==(fst a) ]

testTr ::Eq a => Rel a -> Bool
testTr [] = True
testTr (x:xs) = length([a|a<-(createList (x:xs)), not ((fst x,snd a) `elem` (x:xs))]) == 0 &&  testTr xs


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

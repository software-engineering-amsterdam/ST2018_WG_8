module Lab4 where

import Control.Monad
import Data.List
import Data.Char
import SetOrd
import System.Process
import System.Random
import Test.QuickCheck
import Lecture4
import Control.Monad

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

{-
    Exercise 2 (1 hour):
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
-- Before lists are turned to sets, duplicates are removed and the list is
-- sorted.
instance (Arbitrary a, Ord a) => Arbitrary (Set a) where
    arbitrary = do
        t <- arbitrary
        let t' = sort(nub(t))
        return (Set t')



{-
    Exercise 3:
    Implement operations for set intersection, set union and set difference,
    for the datatype Set defined in SetOrd.hs. Next, use automated testing
    to check that your implementation is correct. First use your own generator,
    next use QuickCheck.
    (Deliverables: implementations, test properties, short test report,
    indication of time spent.)
-}

intersectionSet :: Ord a => Set a -> Set a -> Set a
intersectionSet (Set xs) set2 = Set ([x | x <- xs, inSet x set2])

differenceSet :: Ord a => Set a -> Set a -> Set a
differenceSet (Set xs) (Set ys) = Set (sort((xs \\ ys) ++ (ys \\ xs)))

unionSet' :: (Ord a) => Set a -> Set a -> Set a
unionSet' (Set xs) (Set ys) = Set (sort (nub (xs ++ ys)))

{-
    Test 1:

    Intersection tests that each element of the list is in both result lists.
    In other words the intersection should be a subset of both lists.
    Furthermore, the function should not remove excess items, therefore the same
    list should not loose any items.
-}

testIntersectionSet :: Ord a => Set a -> Set a -> Bool
testIntersectionSet set1 set2 =
    if set1 == set2
        then setLength (intersectionSet set1 set2) == setLength set1
        else crossCheckEls set1 set2

crossCheckEls :: Ord a => Set a -> Set a -> Bool
crossCheckEls set1 set2 = subSet setIntersection set1 && subSet setIntersection set2
    where
        setIntersection = intersectionSet set1 set2

{-
    Test 2:

    The difference should test that each element of the diff list is present in
    only one of the composition lists. Secondly, the diff of a list on itsself should
    be the empty list.
-}

testDifferenceSet :: Ord a => Set a -> Set a -> Bool
testDifferenceSet set1 set2 =
    differenceSet set1 set1 == emptySet && crossCheckUniqueEls set1 set2 setDifference
    where
        setDifference = differenceSet set1 set2

-- Check that the elements from the difference list appears in one list, not two.
crossCheckUniqueEls :: Ord a => Set a -> Set a -> Set a -> Bool
crossCheckUniqueEls set1 set2 (Set xs) = length([x | x <- xs, (inSet x set1) && (inSet x set2)]) == 0

{-
    Test 3:

    unionSet' luckily has a reference function provided in SetOrd (unionSet)
    Nevertheless, I will check that each element of each composition list is in
    the resulting union list.
-}

testUnionSet' :: Ord a => Set a -> Set a -> Bool
testUnionSet' (Set xs) (Set ys) = (setUnion == setUnion') && crossCheckAllEls (Set xs) (Set ys)
    where
        setUnion' = unionSet' (Set xs) (Set ys)
        setUnion = unionSet (Set xs) (Set ys)

crossCheckAllEls :: Ord a => Set a -> Set a -> Bool
crossCheckAllEls (Set xs) (Set ys) =
    xs == [x | x <- xs, inSet x setUnion'] &&
    ys == [y | y <- ys, inSet y setUnion']
    where
        setUnion' = unionSet' (Set xs) (Set ys)

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

type Rel a = [(a,a)]

-- Generates a list of tuples and their inverse to make in symmetrical, then
-- sorts that lits and removes any duplicates.
symClos :: Ord a => Rel a -> Rel a
symClos [] = []
symClos (x:xs) = sort( nub((snd x, fst x) : x : symClos xs))

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
r @@ s =
    nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

-- Use the fixed point function with the findTr function to keep adding transitions
-- untill no new ones can be made.
trClos :: Ord a => Rel a -> Rel a
trClos xs = fp findTr xs

-- Applies the function for making transitions (@@)
findTr :: Ord a => Rel a -> Rel a
findTr xs = sort(nub( (xs @@ xs) ++ xs))

{-
    Exercise 7:
    Test the functions symClos and trClos from the previous exercises.
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

-- Tests if symmetry and transitivity is equal for certain sets.
testSymTrEquality (Set xs) (Set ys) = trClos (symClos (zippedSet)) == symClos (trClos (zippedSet))
    where
        zippedSet = zip xs ys

-- Example that proves difference:
-- This generates a zippedSet of (1,2) which has a symmetry of [(1,2),(2,1)]
-- which in turn has a transitivity of [(1,1),(1,2),(2,1),(2,2)]
-- The inverse of this (first tr then sym produces) [(1,2)] and subsequently
-- [(1,2),(2,1)]. So they are inequal as proved by the underneath function.
setInEquality = testSymTrEquality (Set [1]) (Set [2])

{-
    Bonus:
    In the lecture notes, Statement is in class Show, but the show function
    for it is a bit clumsy. Write your own show function for imperative programs.
    Next, write a read function, and use show and read to state some abstract
    test properties for how these functions should behave. Next, use QuickCheck
    to test your implementations.
    (Deliverable: implementation, QuickCheck properties, test report.)
-}

instance Show Statement where
    show (Ass v e) = v ++ " := " ++ show e ++ "\n"
    show (Cond c s s') = "if " ++ show c ++ " then" ++ show s ++ " else "++ show s'
    show (Seq []) = ""
    show (Seq (x:xs)) = show x ++ show (Seq xs)
    show (While c s) = "while " ++ show c ++ " do {\n" ++ show s ++ "}"

instance Show Expr where
    show (I i) = show i
    show (V v) = v
    show (Add e e') = "(" ++ show e ++ " + " ++ show e' ++ ")"
    show (Subtr e e') = "(" ++ show e ++ " - " ++ show e' ++ ")"
    show (Mult e e') = "(" ++ show e ++ " * " ++ show e' ++ ")"

instance Show Condition where
    show (Prp v) = v
    show (Eq e e') = show e ++ " == " ++ show e'
    show (Lt e e') = show e ++ " < " ++ show e'
    show (Gt e e') = show e ++ " > " ++ show e'
    show (Ng c) = "-" ++ show c
    show (Cj (x:xs)) = show x ++ " && " ++ show xs
    show (Dj (x:xs)) = show x ++ " || " ++ show xs

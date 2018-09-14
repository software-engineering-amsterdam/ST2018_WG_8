module Lab2 where

import Data.List
import Data.Char
import System.Random
import System.Process
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Control.Monad

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all


infix 1 -->

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
             p <- getStdRandom random
             ps <- probs (n-1)
             return (p:ps)

data Shape = NoTriangle | Equilateral
           | Isosceles  | Rectangular | Other deriving (Eq,Show)

-- Exercise 1 (Red Curry)
quantileCount :: IO()
quantileCount = do
    putStrLn("Showing quantile sums (in order) for the porbs function")
    a <- floats
    let convertedFloats = map ceiling (map (fromIntegral (4) * ) a)
    let first = length(filter (== 1) convertedFloats)
    let second = length(filter (== 2) convertedFloats)
    let third = length(filter (== 3) convertedFloats)
    let fourth = length(filter (== 4) convertedFloats)
    print (first, second, third, fourth)
    putStrLn("Sum of the quantile contents:" )
    print (first + second + third + fourth)
        where floats = (probs 100000)


-- Exercise 2
whatShape :: Int -> Int -> Int -> Shape
whatShape x y z
    | isNoTriang x y z  = NoTriangle
    | x == y && y == z = Equilateral
    | x == y || z == y || z == x = Isosceles
    | isRect x y z = Rectangular
    | otherwise = Other

isRect, isNoTriang :: Int -> Int -> Int -> Bool
isRect x y z = ((x ^ 2) + (y ^ 2) == z ^ 2) || ((x ^ 2) + (z ^ 2) == y ^ 2) || ((z ^ 2) + (y ^ 2) == x ^ 2)
isNoTriang x y z = not (x + y > z && x + z > y && z + y > x)

-- Exercise 3
-- a)
myList :: [Int]
myList = [-10..10]

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p

p1,q1,p2,q2,p3,q3,p4,q4 :: Int -> Bool
p1 = (\x -> even x && x > 3)
q1 x = even x
p2 = (\x -> even x || x > 3)
q2 x = even x
p3 = (\x -> (even x && x > 3) || even x)
q3 x = even x
p4 x = even x
q4 = (\x -> (even x && x > 3) || even x)
proplist = [p1,p2,p3,p4,q1,q2,q3,q4]
proplistNames = [("p1",p1),("p2",p2),("p3",p3),("p4",p4),("q1",q1),("q2",q2),("q3",q3),("q4",q4)]

-- First we sort the properties according to the quicksort method, based on whether one is stronger than the other.
-- This is essential for the testing function.
sortFunc :: [Int -> Bool] -> [Int -> Bool]
sortFunc [] = []
sortFunc (x:xs) =
    sortFunc [a | a <- xs, stronger myList a x]
    ++ [x]
    ++ sortFunc [a | a <- xs, not (stronger myList a x)]

-- Secondly we sort the functions again, now with the names attached as tuples.
-- this way we also show the properties names in a showable format (String) in decending order.
sortFuncName :: [(String, Int -> Bool)] -> [String]
sortFuncName [] = []
sortFuncName (x:xs) =
    sortFuncName [a | a <- xs, stronger myList (snd a) (snd x)]
    ++ [fst x]
    ++ sortFuncName [a | a <- xs, not (stronger myList (snd a) (snd x))]

-- Now we can feed the function a number and generate a list of bools depending on which function accepts the number (n)
executeFuncs :: [Int -> Bool] -> Int -> [Bool]
executeFuncs [] _ = []
executeFuncs (x:xs) n = (x n) : executeFuncs xs n

-- Finally, in order to test whether the funcitons are actually sorted, they
-- should be equal to the sorted version of themselves.
-- The Falses should be in the front of the list so if the properties are sorted
-- right, they should always produce the same result as the sorted boolean list.
-- Test idea credits: Sjoerd
testIfOrdered = \x -> (executeFuncs (sortFunc(proplist)) x) == sort (executeFuncs (sortFunc(proplist)) x)


-- Exercise 4
-- Sort the input lists and then compare them.
isPermutation :: Ord a => [a] -> [a] -> Bool
isPermutation list1 list2 = compareLists sorted1 sorted2
    where
        sorted1 = sort(list1)
        sorted2 = sort(list2)

-- Comparison should check each element against the other and check if both lists are empty at the same time.
compareLists :: Ord a => [a] -> [a] -> Bool
compareLists [] [] = True
compareLists x [] = False
compareLists [] y = False
compareLists (x:xs) (y:ys) = x == y && compareLists xs ys


randomList :: Int -> Int -> IO([Int])
randomList n x = replicateM n $ randomRIO (-x,x)

comparePermlists :: Int -> IO Bool
comparePermlists x = do
    -- Generate a random list with a range of x and get a permutation of it.
    let list = randomList 100 x
    li <- list
    let permt = head (permutations(li))
    return (compareLists li permt)

-- Use some monad-magic to make the IO bool output compatible with quickCheck.
testPermlists :: Int -> Property
testPermlists n = monadicIO $ do
    values <- run (comparePermlists n)
    assert (values == True)

-- Exercise 5
-- The derangement defines that all elements of list a should be in list b.
-- However, no element may be in the same place as the other. This means,
-- the lists do not have recurring elements. Properties are that the lists
-- must have equal length and from start through finish no element may occur
-- in the same location. The empty list is by definition a derangement because
-- no elements occur in the same spot. Moreover, each element should be inside
-- the other list. Therefore the properties are: if both lists contain nothing
-- return true, if any of the lists contain something while the other does not
-- return false. Finally and most importantly, x and y may never be the same.

isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement [] [] = True
isDerangement x [] = False
isDerangement [] y = False
isDerangement (x:xs) (y:ys) = (x /= y) && isDerangement xs ys

-- From the definition of derangements, the amount of derangements from a list
-- with n elements is !n (or subfactorial n). In order to test this function
-- I want to have a function that sums all the trues of isDerangement on the
-- permutations of a list. This should be no larger or smaller than !n.
-- The following funciton comes from rosettacode.
subfactorial :: (Eq a, Num a) => a -> a
subfactorial 0 = 1
subfactorial 1 = 0
subfactorial n = (n - 1) * (subfactorial (n - 1) + subfactorial (n - 2))

-- Because generating and working with permutations of lists over 12 becomes
-- unworkable, for the purpose of testing this function I will stick with the
-- lists up to 9.
testlists :: [[Int]]
testlists = permutations [1..9]

-- Accumulate (acc) all lists for which true is returned from the isDerangement func.
-- If the list of permutations is empty, check if the sum of derangements by function
-- equals the factorial of 9 (length of the inputlists). If so, isDerangement works
-- properly for lists up to 9 (ive tested it up untill 10, this is just for speeds sake)
testDerangements :: Int -> [[Int]] -> Bool
testDerangements acc [] = acc == subfactorial 9
testDerangements acc (x:xs) =
    if isDerangement [1..9] x
    then testDerangements (acc + 1) xs
    else testDerangements (acc) xs

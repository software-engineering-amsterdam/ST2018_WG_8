module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck
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
        where floats = (probs 100)

-- Sum of the quantiles does not add up to 10k. No matter the amount of 0's.
-- Although the distribution seems quite random but around 2500, the best way
-- to provide proof is by statistics.

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
-- right, they should always produce the same result as the sorter boolean list
testIfOrdered = \x -> (executeFuncs (sortFunc(proplist)) x) == sort (executeFuncs (sortFunc(proplist)) x)


-- Exercise 4
isPermutation :: Eq => [a] -> [a] -> Bool
isPermutation list1 list2 = compareLists sorted1 sorted2
    where
        sorted1 = sort(list1)
        sorted2 = sort(list2)

compareLists :: Eq => [a] -> [a] -> Bool
compareLists [] [] = True
compareLists x [] = False
compareLists [] y = False
compareLists (x:xs) (y:ys) = x == y && compareLists xs ys

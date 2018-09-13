
module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
             p <- getStdRandom random
             ps <- probs (n-1) 
             return (p:ps)

data Shape = NoTriangle | Equilateral | Isosceles  | Rectangular | Other deriving (Eq,Show)

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

		   
---------------------1
---------------------2 Recognizing triangles

--function which find the correct shape
triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c
    |notrianglefunc a b c = NoTriangle
    |equilateralfunc a b c = Equilateral
    |isoscelesfunc a b c = Isosceles
    |rectangularfunc a b c = Rectangular
    |otherTrianglefunc a b c = Other  

notrianglefunc, equilateralfunc, rectangularfunc, isoscelesfunc, otherTrianglefunc :: Integer->Integer->Integer->Bool

notrianglefunc    a b c = ((a+b)<c)||((a+c)<b)||((b+c)<a)
--condition a==b==c 
equilateralfunc   a b c = (a==b) && (b==c) && (not (notrianglefunc a b c))
-- Pythagorean theorem
rectangularfunc   a b c = (not (notrianglefunc a b c)) && ((a^2 +b^2==c^2)||(c^2 +b^2==a^2)||(a^2 +c^2==b^2))
--Two of the sides must be equal, but not all of them
isoscelesfunc     a b c = (not (notrianglefunc a b c)) && (not (equilateralfunc a b c)) && ((a==b)||(b==c)||(a==c))
--it must be a triangle but not from one of the above categories
otherTrianglefunc a b c = (not (notrianglefunc a b c)) && (not (equilateralfunc a b c)) && (not (rectangularfunc a b c)) && (not (isoscelesfunc a b c))

---------------------3 Testing properties strength
--p= func (a->Bool), q=func (a->Bool), xs=list
stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p

propA1 :: Integer->Bool
propA1 = \x -> ((even x) && (x>3))

propA2 :: Integer->Bool
propA2 x = even x 

propB1 :: Integer->Bool
propB1 = \x -> (((even x) || (x>3)) || even x) 

propB2 :: Integer->Bool
propB2 x = even x 

propC1 :: Integer->Bool
propC1 = \ x -> (((even x) && (x > 3)) || even x)

propC2 :: Integer->Bool
propC2 x = even x

propD1 :: Integer->Bool
propD1 = \ x -> (((even x) && (x > 3)) || even x)

propD2 :: Integer->Bool
propD2 = \ x -> (((even x) && (x > 3)) || even x)

myList = [-10..10]
propList = [propA1,propA2,propB1, propB2, propC1, propC2, propD1, propD2]


--function which sorts the properties (functions)
sortFunc :: [Integer->Bool]->[Integer->Bool]
sortFunc [] = []
sortFunc (x:xs) = 
    sortFunc[a|a<-xs, stronger myList a x] --for every a in xs, if a is stronger than x(head) then make it the head of the list
	++[x]
	++sortFunc[a|a<-xs, weaker myList a x]
	
funcList :: [Integer->Bool]->Integer->[Bool]
funcList [] _ = []
funcList (x:xs) n = (x n) : funcList xs n

-----------------------4 Recognizing Permutations

quicksrt :: Ord a => [a] -> [a]
quicksrt [] = []
quicksrt (x:xs) =
    quicksrt [ a | a <- xs, a <= x ]
    ++ [x]
    ++ quicksrt [ a | a <- xs, a > x ]

isPermutation :: Ord a => [a] -> [a] -> Bool
isPermutation list1 list2 = checkEq (quicksrt list1) (quicksrt list2)

checkEq :: Eq a => [a] -> [a] -> Bool
checkEq [][] = True
checkEq x[] = False
checkEq []y = False
checkEq (x:xs) (y:ys) = x==y && checkEq xs ys

--------------------5 Recognizing and generating derangements

isDerangement :: Ord a => [a] -> [a] -> Bool
isDerangement list1 list2 = (isPermutation list1 list2) && (checkNotEq list1 list2)

checkNotEq :: Eq a => [a] -> [a] -> Bool
checkNotEq [][] = True
checkNotEq x[] = True
checkNotEq []y = True
checkNotEq (x:xs) (y:ys) = not(x==y) && checkNotEq xs ys

deran :: Ord a => [a] -> [[a]]
deran [] = []
deran l = [ x | x <- (permutations l), (isDerangement x l)]






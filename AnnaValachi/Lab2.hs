
module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck
import Data.Ord 

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

--function which finds the correct shape
triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c
    |notrianglefunc a b c = NoTriangle
    |equilateralfunc a b c = Equilateral
    |isoscelesfunc a b c = Isosceles
    |rectangularfunc a b c = Rectangular
    |otherTrianglefunc a b c = Other  

notrianglefunc, equilateralfunc, rectangularfunc, isoscelesfunc, otherTrianglefunc :: Integer->Integer->Integer->Bool

-- i am not using this "not (x + y > z && x + z > y && z + y > x) && x > 0 && y > 0 && z > 0" cause i want to have this answer even if the ints are <0
notrianglefunc    a b c = ((a+b)<c)||((a+c)<b)||((b+c)<a)||a<0||b<0||c<0
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

propA1 :: Int->Bool
propA1 = \x -> ((even x) && (x>3))

propA2 :: Int->Bool
propA2 x = even x 

propB1 :: Int->Bool
propB1 = \x -> (((even x) || (x>3)) || even x) 

propB2 :: Int->Bool
propB2 x = even x 

propC1 :: Int->Bool
propC1 = \ x -> (((even x) && (x > 3)) || even x)

propC2 :: Int->Bool
propC2 x = even x

propD1 :: Int->Bool
propD1 = \ x -> (((even x) && (x > 3)) || even x)

propD2 :: Int->Bool
propD2 = \ x -> (((even x) && (x > 3)) || even x)

myList = [-10..10]

--creating a tuples list. The firdt element is the name of the property and the second the property as a function in haskell
propList = [("propA1",propA1),("propA2",propA2),("propB1",propB1), ("propB2",propB2), ("propC1",propC1), ("propC2",propC2), ("propD1",propD1), ("propD2",propD2)]


--function which sorts the properties 
sortStringProp :: [(String,Int->Bool)]->[(String,Int->Bool)]
sortStringProp [] = []
sortStringProp (x:xs) = 
    sortStringProp[a|a<-xs, stronger myList (snd a) (snd x)] --for every a (a=(String,Integer->Bool)) in xs, if a(Bool) is stronger than x(head) then make it the head of the list
	++[(fst x, snd x)]
	++sortStringProp[a|a<-xs, weaker myList (snd a) (snd x)]
	
funcList :: [(String,Int->Bool)]->Int->[(String,Bool)]
funcList [] _ = []
funcList (x:xs) n = (fst x, ((snd x) n)) : funcList xs n

tuplesort :: [(String,Bool)]->[(String,Bool)]
tuplesort xs = sortBy (comparing snd) xs

testOrder = \x -> (funcList (sortStringProp(propList)) x) == tuplesort (funcList (sortStringProp(propList)) x)

-----------------------4 Recognizing Permutations
--in order to find if the two lists have the same elements, i sort them and then i compare their elements one by one
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
--the two lists must have exactly the same elements but in different positions, so list2 must be a permutation of list1 but do not have any elements in the same position as list1
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


--------------------7 Iban








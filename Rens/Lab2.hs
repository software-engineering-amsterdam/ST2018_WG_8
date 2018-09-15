module Lab2 where

import Data.List
import Data.Char
import System.Random
import System.Process
import System.IO.Unsafe
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


-- Exercise 1
-- This function generates floats using the probs function and multiplies them by 4.
-- This makes it possible to round the numbers to integers and count the occurrences
-- of numbers 1-4. This gives a count of the numbers in each quantile.
quantileCount :: IO(Int, Int, Int, Int)
quantileCount = do
    a <- floats
    let convertedFloats = map ceiling (map (fromIntegral (4) * ) a)
    let first = length(filter (== 1) convertedFloats)
    let second = length(filter (== 2) convertedFloats)
    let third = length(filter (== 3) convertedFloats)
    let fourth = length(filter (== 4) convertedFloats)
    return (first, second, third, fourth)
        where floats = (probs 1000)

-- Execute a test that generates n lists of floats and averages the quantiles.
-- The result is quite consistant with lists of 1000 floats, each quantile ususally
-- contains 250-249 floats which changes every time.
testN = 1000
doQuantileCountTest = testQuantileCount (0,0,0,0) testN

testQuantileCount :: (Int, Int, Int, Int) -> Int -> IO ()
testQuantileCount (acc1, acc2, acc3, acc4) 0 = do
    print (acc1 `div` testN, acc2 `div` testN, acc3 `div` testN, acc4 `div` testN)
testQuantileCount (acc1, acc2, acc3, acc4) n = do
    r <- quantileCount
    let acc1' = acc1 + (first r)
    let acc2' = acc2 + (second r)
    let acc3' = acc3 + (third r)
    let acc4' = acc4 + (fourth r)
    testQuantileCount (acc1', acc2', acc3', acc4') (n - 1)

-- Simple helper funcs to extract elements from a 4-tuple.
first (x, _, _, _) = x
second (_, x, _, _) = x
third (_, _, x, _) = x
fourth (_, _, _, x) = x


-- Exercise 2
-- I decided to implement the notriangle property first, this is the strongest
-- property as all the others should be triangles and the sets of triangles and
-- nottriangles are by definition disjunct. The second property is the equilateral
-- property. Once again, the equilateral comes before isosceles because all equilateral
-- triangles are by definition isosceles (if 3 sides are the same length then
-- 2 of them are also the same length of course). Because isoceles triangles require
-- 2 sides equal only, the equilaterals are a subset of the isoceles. The isosceles
-- property is thus weaker. Finally, the rectangular property is tested, this property
-- has no overlap with the other properties as for equilateral no 3 numbers exist
-- that 2 * k^2 = k^2. They can also be isosceles (2,2,sqrt(8) for example) but
-- they are no subsets of eachother so I've given priority to the isoceles.

whatShape :: Int -> Int -> Int -> Shape
whatShape x y z
    | isNoTriang x y z  = NoTriangle
    | x == y && y == z = Equilateral
    | x == y || z == y || z == x = Isosceles
    | isRect x y z = Rectangular
    | otherwise = Other

-- isRect checks if any comination of the integers fits the pythagorean theorem.
-- isNoTriang checks if any dimension is subzero or if the sum of any of two
-- sides exceeds the remaining sides (a triangle cannot be formed then).
isRect, isNoTriang :: Int -> Int -> Int -> Bool
isRect x y z = ((x ^ 2) + (y ^ 2) == z ^ 2) || ((x ^ 2) + (z ^ 2) == y ^ 2) || ((z ^ 2) + (y ^ 2) == x ^ 2)
isNoTriang x y z = not (x + y > z && x + z > y && z + y > x) && x > 0 && y > 0 && z > 0


-- Exercise 3
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

-- Secondly we sort the functions again, now with the names attached in tuples.
-- this way we also show the properties names in a showable format (String) in
-- decending order of stength.
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
-- To check if one list is a permutation of the other we should make sure that
-- each element occurs the same amount of times as each element in the proposed
-- permutation. Furthermore, the lists should be the same length.
-- To check this we sort both lists and compare them element by element.
-- This way we ensure that the elements are exactly the same. If any list reaches
-- its end before the other, we should return false. We can test this function by
-- feeding it generated permutations of randomly generated lists of numbers and then
-- comparing them using compareLists.

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
testLists :: [[Int]]
testLists = permutations [1..9]

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

-- The derangements of a list are a subset of the permutations. Therefore we can
-- say that the derangement property is stronger than the permutation property.


-- Exercise 6
-- Rot13 operates as a ceasar cipher, it rotates letters 13 spaces through the
-- alphabet creating mappings like: abc -> nop. Because the alphabet is 26
-- characters, nop will also map back to abc. The backbone of the function below
-- was copied from rosettacode. Converted for string support.

rot13 :: [Char] -> [Char]
rot13 [] = []
rot13 (c:cs)
  | isAlpha c = chr (if_ (toLower c <= 'm') (+) (-) (ord c) 13) : rot13 cs
  | otherwise = c : rot13 cs

if_ :: Bool -> a -> a -> a
if_ True x _ = x
if_ False _ y = y

-- Due to its inversibility we can test this cipher. First we input a string.
-- Second we test whether the string has changed (it should have), then we turn
-- it back and compare it to the original. These should be the same, case and all.
-- The below strings have
testStrings = ["simpletest", "harDer Test", "123 SuPerTest Which is longer"]

testRot13 :: [[Char]] -> Bool
testRot13 [] = True
testRot13 (x:xs) = ((rot13 x) /= x && (rot13 $ rot13 x) == x) && testRot13 xs


-- Exercise 7
testIban1 = "FR76 3000 6000 0112 3456 7890 189"
testIban2 = "GR96 0810 0010 0000 0123 4567 890"

-- Lets implement the iban validation algorithm. First filter out spaces for
-- ease of parsing. Now move the first 4 digits/letters to the end.
moveLettersToEnd,filterOutSpaces :: [Char] -> [Char]
filterOutSpaces xs = filter (/= ' ') xs
moveLettersToEnd xs = (drop 4 (filterOutSpaces xs)) ++ (take 4 (filterOutSpaces xs))

-- Now for each letter convert it to 10 + its index in the alphabet (resulting in
-- a -87 conversion starting with A at 0).
lettersToNum :: [Char] -> [Char]
lettersToNum [] = ""
lettersToNum (c:cs) =
    if isAlpha c
    then show(ord (toLower c) - 87) ++ (lettersToNum cs )
    else [c] ++ (lettersToNum cs)

-- Finally check if the resulting number mod 97 equals 1, if so, the iban is valid.
-- This does not mean the iban is in use...
validateIBAN :: [Char] -> Bool
validateIBAN xs = (read $ lettersToNum $ moveLettersToEnd $ filterOutSpaces xs) `mod` 97 == 1


-- Now for testing lets build a IBAN generator.
-- We will build a generator for french IBAN numbers.
country, frHead :: String
country = "FR"
frHead = country ++ "00"

-- First generate an ibansized number (without the head (FR00)).
randomIBAN :: IO Integer
randomIBAN = randomRIO(10000000000000000000000,99999999999999999999999)

-- Convert the number to a string with the proper head.
numToIBAN :: IO String
numToIBAN = do
    a <- (show <$> randomIBAN)
    return a

generateCorrectIBAN :: IO Integer -> IO [Char]
generateCorrectIBAN n = do
    -- Generate a basic IBAN format number and add the head at the end.
    base <- n
    let baseWithHead = ((show $ base) ++ frHead)

    -- Transform letters into numbers
    let convLetBase = lettersToNum baseWithHead

    -- Turn back into Integer (Add 0 to avoid ambiguous typing).
    let ibanNum = 0 + (read $ convLetBase)

    -- Now mod the iban with 97, subtract 98 and replace the check digit with it.
    let remainder = ibanNum `mod` 97

    -- If the check digit is <10 add a zero for padding.
    let newHead = country ++ addZeroPadding(show (98 - remainder))
    let newIban = newHead ++ (show base)
    putStrLn("Generated IBAN: " ++ newIban)
    return newIban

-- Adds padding to strings containing a sub 10 number. (Maintains iban length)
addZeroPadding :: String -> String
addZeroPadding s
    | (read s) < 10 = "0" ++ s
    | otherwise = s

-- Tests if the generated IBANs are accepted, does this n times.
testIBANvalidator :: Integer -> IO Bool
testIBANvalidator 0 = do
    return True
testIBANvalidator n = do
    -- Get the test result.
    testResult <- (validateIBAN <$> (generateCorrectIBAN randomIBAN))

    -- Generate the other results recursively.
    let results = (testIBANvalidator (n - 1))
    boolOthers <- results

    -- Returns true if all results are True.
    return ((testResult &&) boolOthers)

-- Simple function to print the result of the iban test in the main function.
printIOBool :: IO Bool -> IO ()
printIOBool b = do
    b0 <- b
    print b0

main :: IO()
main = do
    putStrLn("Testing if the quantiles are evenly spread over 1000 samples 1000 times:")
    doQuantileCountTest
    putStrLn("Should output around 250 each. (or 249 due to rounding method.)")

    putStrLn("\nOrdering properies stronger->weaker:")
    print $ sortFuncName proplistNames
    putStrLn("Testing whether ordering is correct with quickCheck inputting different domains:")
    quickCheck testIfOrdered

    putStrLn("\nTest if we can correctly detect if a list is a permutation of another list:")
    quickCheck testPermlists

    putStrLn("\nTest if we can correctly detect if a list is a derangement of another list:")
    print $ testDerangements 0 testLists

    putStrLn("\nTest if the rot13 cipher is functioning properly with string input:")
    print $ testRot13 testStrings

    putStrLn("\nTest if generated IBANs are validated by the validateIBAN function:")
    printIOBool (testIBANvalidator 10)
    putStrLn("\nDone!")

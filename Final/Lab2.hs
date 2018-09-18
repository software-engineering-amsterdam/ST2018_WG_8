
module Lab2 where

import Control.Monad
import Data.Char
import Data.List
import Data.Ord 
import System.Random
import System.Process
import System.IO.Unsafe
import Test.QuickCheck
-- import Test.QuickCheck.Monadic -- TO DO: Is this needed?

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

{- 
    Assignment 1: Test the float generator.
    Since (0.00 - 0.25) and (0.75 - 1.00) are sets with 1 float extra, perhaps
    you could see this back in the quartile distribution. I tested it with 
    100.000 random floats, but this was not seen in the results.
-}

-- Given function to randomly generate floats between 0.0 and 1.0.
probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
            p <- getStdRandom random
            ps <- probs (n - 1) 
            return (p:ps)
            
-- Function that creates 10.000 floats and then filters them by value per quartile. 
-- It returns the size of each list (4 in total) which should all be around 2500.            
testProbs :: IO [Int]
testProbs = do 
                sample <- probs 10000
                return ([length [x | x <- sample, x > 0 && x < 0.25],
                         length [x | x <- sample, x >= 0.25 && x < 0.50],
                         length [x | x <- sample, x >= 0.50 && x < 0.75],
                         length [x | x <- sample, x >= 0.75 && x < 1]])

{-
    Assignment 2: Write a program (in Haskell) that takes a triple of integer
    values as arguments, and gives as output one of the following statements:
    Not a triangle, Equilateral, Rectangular, Isosceles or Other.
    Deliverables: Haskell program, concise test report, indication of time spent.
-}

-- The given data Shape.
data Shape = NoTriangle | Equilateral | Isosceles | Rectangular | Other
    deriving (Eq, Show)

-- Function that checks the triplet for each of the possible triangles and their conditions.
triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c 
    | ( a > b + c) || (b > c + a) || (a < c + b) || (a < 0 || b < 0 || c < 0)= NoTriangle
    | (a == b) && (b == c) = Equilateral
    | ((a == b) && (a /= c)) || ((a == c) && (a /= b)) || ((b == c) && (b /= a)) = Isosceles
    | (a^2 + b^2 == c^2) || (b^2 + c^2 == a^2) || (a^2 + c^2 == b^2) = Rectangular
    | otherwise = Other

-- TO DO: TESTING!!!!
-- Aynel did quickcheck testing

    
{- 
    Assignment 3. Implement all properties from the Exercise 3 from Workshop 2
    as Haskell functions of type Int -> Bool. Consider a domain like [(−10)..10].
    b) Provide a descending strength list of all the implemented properties.
-}

-- Given functions to check which property is stronger or weaker.
stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p

propA1, propA2, propB1, propB2 :: Int->Bool
propA1 = \x -> ((even x) && (x>3))
propA2 x = even x 
propB1 = \x -> (((even x) || (x>3)) || even x) 
propC1 = \ x -> (((even x) && (x > 3)) || even x)

myList = [-10..10]

-- Create a tuple list of (name, property).
propList = [("propA1", propA1),("propA2", propA2),("propB1", propB1), ("propC1", propC1)]

-- Function to sort the tuples based on the properties in a quickSort manner.
sortStringProp :: [(String, Int -> Bool)] -> [(String, Int -> Bool)]
sortStringProp [] = []
sortStringProp (x:xs) = 
    sortStringProp [ a | a <- xs, stronger myList (snd a) (snd x)] 
    ++ [(fst x, snd x)]
    ++ sortStringProp [ a | a <- xs, weaker myList (snd a) (snd x)]

-- Function to transform the tuples into (name, values) for a certain domain.
funcList :: [(String, Int -> Bool)] -> Int -> [(String, Bool)]
funcList [] _ = []
funcList (x:xs) n = (fst x, ((snd x) n)) : funcList xs n

-- Function to sort the tuples with a Haskell built-in function.
tupleSort :: [(String, Bool)] -> [(String, Bool)]
tupleSort xs = sortBy (comparing snd) xs

-- Function to test our own sorting algorithm with the Haskell function.
testOrder = \x -> (funcList (sortStringProp(propList)) x) == tuplesort (funcList (sortStringProp(propList)) x)

{-
    Assignment 4: Recognizing Permutations
    Create a function that returns True if its arguments are permutations of each other.
    Next, define some testable properties for this function,
    and use a number of well-chosen lists to test isPermutation.
    What does this mean for your testing procedure?
    Deliverables: Haskell program, concise test report, indication of time spent.
-}

-- Sort the input lists and then compare both lists with each other.
isPermutation :: Ord a => [a] -> [a] -> Bool
isPermutation list1 list2 = compareLists sorted1 sorted2
    where
        sorted1 = sort(list1)
        sorted2 = sort(list2)

-- Function to check two lists against each other.
-- On success, return true. Else, return false.
compareLists :: Ord a => [a] -> [a] -> Bool
compareLists [] [] = True
compareLists x [] = False
compareLists [] y = False
compareLists (x:xs) (y:ys) = x == y && compareLists xs ys

-- Function to generate permutations of randomly generated lists of numbers.
randomList :: Int -> Int -> IO([Int])
randomList n x = replicateM n $ randomRIO (-x,x)

-- Function to compare generated lists using compareLists.
comparePermlists :: Int -> IO Bool
comparePermlists x = do
    let list = randomList 100 x
    li <- list
    let permt = head (permutations(li))
    return (compareLists li permt) -- TO DO: Something was wrong here? 

-- Use some monad-magic to make the IO bool output compatible with quickCheck.
testPermlists :: Int -> Property
testPermlists n = monadicIO $ do
    values <- run (comparePermlists n)
    assert (values == True)

{- 
    Assignment 5: Give a Haskell implementation of a property isDerangement.
    Give a Haskell implementation of a function deran.
    Can you automate the test process?
    Deliverables: Haskell program, concise test report, indication of time spent.
-}

-- isDerangement creates two permutations of the same list.
-- The lists must have exactly the same elements but in different positions,
isDerangement :: Ord a => [a] -> [a] -> Bool
isDerangement list1 list2 = (isPermutation list1 list2) && (checkNotEq list1 list2)

-- Function to check if two elements in the list are the same.
checkNotEq :: Eq a => [a] -> [a] -> Bool
checkNotEq [] [] = True
checkNotEq x [] = True
checkNotEq [] y = True
checkNotEq (x:xs) (y:ys) = not (x == y) && checkNotEq xs ys

-- From the definition of Derangements, the amount of derangements from a list
-- with n elements is !n (or subfactorial n). Found this on Rosettacode.
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
-- equals the factorial of 9 (length of the inputlists). 
-- If so, isDerangement works properly for lists up to 9.
testDerangements :: Int -> [[Int]] -> Bool
testDerangements acc [] = acc == subfactorial 9
testDerangements acc (x:xs) =
    if isDerangement [1..9] x
    then testDerangements (acc + 1) xs
    else testDerangements (acc) xs

-- The derangements of a list are a subset of the permutations. Therefore we can
-- say that the derangement property is stronger than the permutation property.


{- 
    Assignment 6: First, give a specification of ROT13.
    Next, give a simple implementation of ROT13.
    Finally, turn the specification into a series of QuickCheck
    testable properties, and use these to test your implementation.
-}

-- Rot13 operates as a ceasar cipher, it rotates letters 13 spaces.
-- The backbone of the function was from rosettacode. Converted for string support.

-- Function to rotate each letter in the list 13 spaces if it's a letter.
rot13 :: [Char] -> [Char]
rot13 [] = []
rot13 (c:cs)
  | isAlpha c = chr (if' (toLower c <= 'm') (+) (-) (ord c) 13) : rot13 cs
  | otherwise = c : rot13 cs

-- Function to create own ternary operator in Haskell.
if' :: Bool -> a -> a -> a
if' True x _ = x
if' False _ y = y

-- Due to its inversibility we can test this cipher. First we input a string.
-- Second we test whether the string has changed (it should have), then we turn
-- it back and compare it to the original. These should be the same, case and all.
-- The below strings have
testStrings = ["simpletest", "harDer Test", "123 SuPerTest Which is longer"]

testRot13 :: [[Char]] -> Bool
testRot13 [] = True
testRot13 (x:xs) = ((rot13 x) /= x && (rot13 $ rot13 x) == x) && testRot13 xs
-- TO DO: Look at this a bit more.


{- 
    Exercise 7: Write a function iban :: String -> Bool
    that implements the validation procedure.
    Next, test your implementation using some suitable list of examples.
    It is not enough to test only with correct examples.
    You should invent a way to test with incorrect examples also.
    Can you automate the test process?
    Deliverables: Haskell program, concise test report, indication of time spent.
-}
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


module Lab2 where

import Control.Monad
import Data.Char
import Data.List
import Data.Ord
import System.Random
import System.Process
import System.IO.Unsafe
import Test.QuickCheck
import Test.QuickCheck.Monadic

infix 1 -->

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

{-
    Exercise 1: Test the float generator.
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
    Exercise 2: With 3 integers, give as output one of the following statements:
    Not a triangle, Equilateral, Rectangular, Isosceles or Other.
    Deliverables: Haskell program, concise test report, indication of time spent.
-}

-- The given data Shape.
data Shape = NoTriangle | Equilateral | Isosceles | Rectangular | Other
    deriving (Eq, Show)

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c
    | notTriangleF a b c = NoTriangle
    | equilateralF a b c = Equilateral
    | isoscelesF a b c = Isosceles
    | rectangularF a b c = Rectangular
    | otherF a b c = Other  

-- Function to match the properties to the right triangle.
notTriangleF, equilateralF, rectangularF, isoscelesF, otherF :: Integer->Integer->Integer->Bool
notTriangleF a b c = a + b < c || a + c < b || b + c < a || a < 0 || b < 0 || c < 0
equilateralF a b c = a == b && b == c && not (notTriangleF a b c)
rectangularF a b c = not (notTriangleF a b c) && 
    (a^2 + b^2 == c^2 || c^2 + b^2 == a^2 || a^2 + c^2 == b^2)
isoscelesF a b c = not (notTriangleF a b c) && not (equilateralF a b c) &&
    (a == b || b == c || a == c)
otherF a b c = not (notTriangleF a b c) && not (equilateralF a b c) &&
    not (rectangularF a b c) && not (isoscelesF a b c)

-- Test functions below. They use the conditions of the definitions above,
-- so this is not the best way to test. You would rather check other 
-- mathematical properties such as the correct angles, but this is quite hard
-- in Haskell with just the lengths of the vertices.
testNoTriangles1, testNoTriangles2, testNoTriangles3 :: Integer -> Integer -> Integer -> Bool
testNoTriangles1 a b c = (a > b + c) --> triangle a b c == NoTriangle
testNoTriangles2 a b c = (b > c + a) --> triangle a b c == NoTriangle
testNoTriangles3 a b c = (c > a + b) --> triangle a b c == NoTriangle

testIsosceles1, testIsosceles2, testIsosceles3 :: Integer -> Integer -> Bool
testIsosceles1 a b = (a > 0 && b > 0 && a /= b &&
    not (notTriangleF a a b)) --> triangle a a b == Isosceles
testIsosceles2 a b = (a > 0 && b > 0 && a /= b &&
    not (notTriangleF b a a)) --> triangle b a a == Isosceles
testIsosceles3 a b = (a > 0 && b > 0 && a /= b &&
    not (notTriangleF a b a)) --> triangle a b a == Isosceles

testRectangular1, testRectangular2, testRectangular3 :: Integer -> Integer -> Integer -> Bool
testRectangular1 a b c = (a > 0 && b > 0 && c > 0 && a^2 + b^2 == c^2) 
                        --> triangle a b c == Rectangular
testRectangular2 a b c = (a > 0 && b > 0 && c > 0 && b^2 + c^2 == a^2) 
                        --> triangle a b c == Rectangular
testRectangular3 a b c = (a > 0 && b > 0 && c > 0 && a^2 + c^2 == b^2) 
                        --> triangle a b c == Rectangular

testEquilateral :: Integer -> Bool
testEquilateral a = (a > 0) --> triangle a a a == Equilateral

{-
    Exercise 3: Implement all properties from the Exercise 3 from Workshop 2
    as Haskell functions of type Int -> Bool. Consider a domain like [(−10)..10].
    Provide a descending strength list of all the implemented properties.
-}

-- Given functions to check which property is stronger or weaker.
stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p

propA1, propA2, propB1, propC1 :: Int -> Bool
propA1 = \x -> ((even x) && (x>3))
propA2 x = even x
propB1 = \x -> (((even x) || (x>3)) || even x)
propC1 = \ x -> (((even x) && (x > 3)) || even x)

myList = [-10..10]

-- Create a tuple list of (name, property).
propList = [("propA1", propA1),("propA2", propA2),
    ("propB1", propB1), ("propC1", propC1)]

-- Function to sort the tuples based on the properties in a quickSort manner.
sortTuples :: [(String, Int -> Bool)] -> [(String, Int -> Bool)]
sortTuples [] = []
sortTuples (x:xs) =
    sortTuples [ a | a <- xs, stronger myList (snd a) (snd x)]
    ++ [(fst x, snd x)]
    ++ sortTuples [ a | a <- xs, weaker myList (snd a) (snd x)]

-- Function to transform the tuples into (name, values) for a certain domain.
funcList :: [(String, Int -> Bool)] -> Int -> [(String, Bool)]
funcList [] _ = []
funcList (x:xs) n = (fst x, ((snd x) n)) : funcList xs n

-- Function to sort the tuples with a Haskell built-in function.
tupleSort :: [(String, Bool)] -> [(String, Bool)]
tupleSort xs = sortBy (comparing snd) xs

-- Function to test our own sorting algorithm with the Haskell function.
testOrder = \x -> (funcList (sortTuples(propList)) x) ==
             tupleSort (funcList (sortTuples(propList)) x)

{-
    Exercise 4: Create a function that returns True when its arguments are permutations of each other.
    Define some testable properties for this function.
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
    let permt = head (permutations (li))
    return (isPermutation li permt)

-- Use some monad-magic to make the IO bool output compatible with quickCheck.
testPermlists :: Int -> Property
testPermlists n = monadicIO $ do
    values <- run (comparePermlists n)
    assert (values == True)

{-
    Exercise 5: Give a Haskell implementation of a property isDerangement.
    Give a Haskell implementation of a function deran.
    Can you automate the test process?
    Deliverables: Haskell program, concise test report, indication of time spent.
-}


-- The lists must have exactly the same elements but in different positions.
isDerangement :: Ord a => [a] -> [a] -> Bool
isDerangement list1 list2 = (isPermutation list1 list2) && (checkNotEq list1 list2)

-- Function to check if two elements in the list are not the same.
checkNotEq :: Eq a => [a] -> [a] -> Bool
checkNotEq [] [] = True
checkNotEq x [] = True
checkNotEq [] y = True
checkNotEq (x:xs) (y:ys) = not (x == y) && checkNotEq xs ys

deran :: Ord a => [a] -> [[a]]
deran [] = []
deran l = [ x | x <- (permutations l), (isDerangement x l)]

-- From the definition of Derangements, the amount of derangements from a list
-- with n elements is !n (or subfactorial n). Found this on Rosettacode.
subfactorial :: (Eq a, Num a) => a -> a
subfactorial 0 = 1
subfactorial 1 = 0
subfactorial n = (n - 1) * (subfactorial (n - 1) + subfactorial (n - 2))

testLists :: [[Int]]
testLists = permutations [1..9]

-- Function to accumulate all lists for which isDerangement == True.
-- If the list of permutations is empty, check if the sum of derangements by function
-- equals the factorial of 9 (length of the inputlists).
-- If so, isDerangement works properly for lists up to 9.
testDerangements :: Int -> [[Int]] -> Bool
testDerangements acc [] = acc == subfactorial 9
testDerangements acc (x:xs) =
    if isDerangement [1..9] x
    then testDerangements (acc + 1) xs
    else testDerangements (acc) xs

{-
    Exercise 6: First, give a specification of ROT13.
    Next, give a simple implementation of ROT13.
    Finally, turn the specification into a series of QuickCheck
    testable properties, and use these to test your implementation.
-}

-- Rot13 operates as a ceasar cipher, it rotates letters 13 spaces.
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

-- Test functions.
testRot13 :: Bool
testRot13 = (rot13("test") /= "test") && (rot13(rot13("test")) == "test")

testSpacing :: Bool
testSpacing = (rot13("te st") /= "te st") && (rot13(rot13("te st")) == "te st")

testNumberSupport :: Bool
testNumberSupport = (rot13("1est") /= "1est") && (rot13(rot13("1est")) == "1est")

testCapitalisation :: Bool
testCapitalisation = (rot13("Test") /= "Test") && (rot13(rot13("Test")) == "Test")

{-
    Exercise 7: Write a function IBAN :: String -> Bool.
    Next, test your implementation using some suitable list of examples.
    Can you automate the test process?
    Deliverables: Haskell program, concise test report, indication of time spent.
-}

testIBAN1 = "FR76 3000 6000 0112 3456 7890 189"
testIBAN2 = "GR96 0810 0010 0000 0123 4567 890"

-- Function to filter out spaces and move the first 4 chars to the end.
moveLettersToEnd, filterOutSpaces :: [Char] -> [Char]
filterOutSpaces xs = filter (/= ' ') xs
moveLettersToEnd xs = (drop 4 (filterOutSpaces xs)) ++ (take 4 (filterOutSpaces xs))

-- Function to convert each char to 10 + its index in the alphabet.
-- This results in a - 87 conversion starting with A at 0).
lettersToNum :: [Char] -> [Char]
lettersToNum [] = ""
lettersToNum (c:cs) =
    if isAlpha c
    then show (ord (toLower c) - 87) ++ (lettersToNum cs )
    else [c] ++ (lettersToNum cs)

-- Function to create the head of French IBANs.
countryCode, frHead :: String
countryCode = "FR"
frHead = countryCode ++ "00"

-- First generate an IBANsized number (without the head (FR00)).
randomIBAN :: IO Integer
randomIBAN = randomRIO(10000000000000000000000, 99999999999999999999999)

-- Check if the resulting number mod 97 equals 1, if so, the IBAN is valid.
validateIBAN :: [Char] -> Bool
validateIBAN xs = (read $ lettersToNum $ moveLettersToEnd $ filterOutSpaces xs) `mod` 97 == 1

generateCorrectIBAN :: IO Integer -> IO [Char]
generateCorrectIBAN n = do
    -- Generate a basic IBAN format number and add the head at the end.
    base <- n
    let baseWithHead = ((show $ base) ++ frHead)

    -- Transform letters into numbers
    let convertedBase = lettersToNum baseWithHead

    -- Transform back into integer (add 0 to avoid ambiguous typing).
    let ibanNum = 0 + (read $ convertedBase)

    -- Now mod the IBAN with 97, subtract 98 and replace the check digit with it.
    let remainder = ibanNum `mod` 97

    -- If the check digit is <10 add a zero for padding.
    let newHead = countryCode ++ addPadding (show (98 - remainder))
    let newIBAN = newHead ++ (show base)
    putStrLn("Generated IBAN: " ++ newIBAN)
    return newIBAN

-- Adds padding to strings containing a sub 10 number. (Maintains IBAN length)
addPadding :: String -> String
addPadding s
    | (read s) < 10 = "0" ++ s
    | otherwise = s

-- Function the test if the generated IBANs are accepted, do this n times.
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

-- Function to print the result of the IBAN test in the main function.
printIOBool :: IO Bool -> IO ()
printIOBool b = do
    b0 <- b
    print b0

main :: IO()
main = do
    putStrLn("Testing exercise 1 with 10.000 samples (should be +- 2500 each):")
    exercise1 <- testProbs
    print exercise1

    putStrLn("Testing exercise 2.")
    putStrLn("Checking Equilateral:")
    quickCheck testEquilateral

    putStrLn("Checking Isosceles:")
    quickCheck testIsosceles1
    quickCheck testIsosceles2
    quickCheck testIsosceles3

    putStrLn("Checking Rectangular:")
    quickCheck testRectangular1
    quickCheck testRectangular2
    quickCheck testRectangular3

    putStrLn("Checking No Triangles:")
    quickCheck testNoTriangles1
    quickCheck testNoTriangles2
    quickCheck testNoTriangles3

    putStrLn("Testing exercise 3 (compare properties' strength).")
    quickCheck testOrder

    putStrLn("Testing exercise 4 (check for permutations).")
    quickCheck testPermlists

    putStrLn("Testing exercise 5 (check for derangements).")
    print $ testDerangements 0 testLists

    putStrLn("Testing exercise 6 (The rot13 cipher).")
    putStrLn ((show testRot13) ++ " for general functioning.")
    putStrLn ((show testCapitalisation) ++ " for capitalisation.")
    putStrLn ((show testSpacing) ++ " for spacing.")
    putStrLn ((show testNumberSupport) ++ " for non-alphabetic support.")

    putStrLn("Testing exercise 7 (French IBANs).")
    printIOBool (testIBANvalidator 10)
    
    putStrLn("Done testing!")

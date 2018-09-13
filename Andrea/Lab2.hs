
module Lab2 where
import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

{-
    Test  by counting the numbers in the quartiles
    (0..0.25),[0.25..0.5),[0.5..0.75),[0.75..1)
    and checking whether the proportions between these are as expected.
    E.g., if you generate 10000 numbers,
    then roughly 2500 of them should be in each quartile.
    Implement this test, and report on the test results.
-}

probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
            p <- getStdRandom random
            ps <- probs (n - 1) 
            return (p:ps)
            
-- testProbs :: Int ->
    

{-
    Assignment 2: Write a program (in Haskell) that takes a triple of integer
    values as arguments, and gives as output one of the following statements:
    Not a triangle, Equilateral, Rectangular, Isosceles or Other.
    Deliverables: Haskell program, concise test report, indication of time spent.
-}

data Shape = NoTriangle | Equilateral | Isosceles | Rectangular | Other
    deriving (Eq, Show)

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c 
    | not (( a < b + c) && (b < c + a) && (a < c + b)) = NoTriangle
    | (a == b) && (b == c) = Equilateral
    | ((a == b) && (a /= c)) || ((a == c) && (a /= b)) || ((b == c) && (b /= a)) = Isosceles
    | (a^2 + b^2 == c^2) || (b^2 + c^2 == a^2) || (a^2 + c^2 == b^2) = Rectangular
    | otherwise = Other

randomTriple :: IO [Integer, Integer, Integer]
randomTriple = do 
                x <- getRandomInt 100
                y <- getRandomInt 100
                z <- getRandomInt 100
                    return [x, y, z]

    -- 10 mins

-- testTriangle :: Integer -> Integer -> Integer -> Bool
{- 
    a) Implement all properties from the Exercise 3 from Workshop 2
    as Haskell functions of type Int -> Bool. Consider a domain like [(−10)..10].
    b) Provide a descending strength list of all the implemented properties.
-}

forall = flip all

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p 

prop1, prop2, prop3, prop4 :: Int -> Bool
prop1 x = even x && x > 3
prop2 x = even x || x > 3
prop3 x = (even x && x > 3) || even x
prop4 x = even x

{-
    Create a function that returns True if its arguments are permutations of each other.

    Next, define some testable properties for this function,
    and use a number of well-chosen lists to test isPermutation.
    You may assume that your input lists do not contain duplicates.
    What does this mean for your testing procedure?

    Provide an ordered list of properties by strength using the weakear and stronger definitions.
    Can you automate the test process? Use the techniques presented in this week's lecture. Also use QuickCheck.
    Deliverables: Haskell program, concise test report, indication of time spent.

-}

-- isPermutation :: Eq a => [a] -> [a] -> Bool
-- isPermutation 






{- 
    Give a Haskell implementation of a property isDerangement that
    checks whether one list is a derangement of another one.
    Give a Haskell implementation of a function deran that generates a list of all derangements of the list [0..n-1].

    Note: You may wish to use the permutations function from Data.List, or the perms function from workshop 1.
    Next, define some testable properties for the isDerangement function, and use some well-chosen integer lists to test isDerangement.
    Provide an ordered list of properties by strength using the weakear and stronger definitions.
    Can you automate the test process?

    Deliverables: Haskell program, concise test report, indication of time spent.
-}

{- 
    First, give a specification of ROT13.
    Next, give a simple implementation of ROT13.
    Finally, turn the specification into a series of QuickCheck
     testable properties, and use these to test your implementation.
-}

-- Rot13 substitutes a letter with another letter 13 (mod 26) in the alphabet.
-- rot13 :: Char -> Char
-- rot13 a = 


{- 
    The International Bank Account Number (IBAN) was designed to facility international money transfer,
    to uniquely identify bank accounts worldwide. It is described here, including a procedure for validating IBAN codes. Write a function

    iban :: String -> Bool

    that implements this validation procedure.

    Next, test your implementation using some suitable list of examples.

    Note It is not enough to test only with correct examples. You should invent a way to test with incorrect examples also.

    Can you automate the test process?

    Deliverables: Haskell program, concise test report, indication of time spent.
-}
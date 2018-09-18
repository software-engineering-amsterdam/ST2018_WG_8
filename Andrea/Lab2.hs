
module Lab2 where
import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

{- 
    Assignment 1: Test the float generator.
    Since (0.00 - 0.25) and (0.75 - 1.00) are sets with 1 float extra, perhaps
    you could see this back in the quartile distribution. I tested it with 
    100.000 random floats, but this was not seen in the results.
-}

-- probs :: given random generator to find floats between 0.0 and 1.0.
probs :: Int -> IO [Float]
probs 0 = return []
probs n = do
            p <- getStdRandom random
            ps <- probs (n - 1) 
            return (p:ps)
            
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

data Shape = NoTriangle | Equilateral | Isosceles | Rectangular | Other
    deriving (Eq, Show)

triangle :: Integer -> Integer -> Integer -> Shape
triangle a b c 
    | not (( a < b + c) && (b < c + a) && (a < c + b)) = NoTriangle
    | (a == b) && (b == c) = Equilateral
    | ((a == b) && (a /= c)) || ((a == c) && (a /= b)) || ((b == c) && (b /= a)) = Isosceles -- not Equilateral
    | (a^2 + b^2 == c^2) || (b^2 + c^2 == a^2) || (a^2 + c^2 == b^2) = Rectangular
    | otherwise = Other

randomTriple :: IO (Integer, Integer, Integer)
randomTriple = do 
                   x <- randomRIO (1, 20)
                   y <- randomRIO (1, 20)
                   z <- randomRIO (x + y, 20 + x + y)
                   return (x, y, z)
 -- 15 mins

 -- TO DO: WRITE TESTS! 

-- testTriangle :: Integer -> Integer -> Integer -> Bool
{- 
    Assignment 3. Implement all properties from the Exercise 3 from Workshop 2
    as Haskell functions of type Int -> Bool. Consider a domain like [(−10)..10].
    b) Provide a descending strength list of all the implemented properties.
-}

forall = flip all

domain :: [Int]
domain = [-10..10]

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p 

prop1, prop2, prop3, prop4 :: Int -> Bool
prop1 x = \x -> even x && x > 3
prop2 x = \x -> even x || x > 3
prop3 x = \x -> (even x && x > 3) || even x
prop4 x = \x -> even x

propertyList = [(prop1, "prop1"), (prop2, "prop2"), (prop3, "prop3"), (prop4, "prop4")]

-- Sort the props using quick sort.
quickSortProps :: [Int -> Bool] -> [Int -> Bool]
quickSortProps [] = []
quickSortProps (x:xs) =
    quickSortProps [a | a <- xs, stronger myList a x]
    ++ [x]
    ++ quickSortProps [a | a <- xs, not (stronger myList a x)]

quickSortTest ((f,name):xs) = quickSortTest [(f1,n1) | (f1, n1) <- xs, stronger [-10..10] f1 f]
    ++ [name] ++ 
    quickSortTest [(f1,n1) | (f1, n1) <- xs, not (stronger [-10..10] f1 f)]

-- strengthList = sortTest propertyList



{-
    Assignment 4: Recognizing Permutations
    Create a function that returns True if its arguments are permutations of each other.

    Next, define some testable properties for this function,
    and use a number of well-chosen lists to test isPermutation.
    You may assume that your input lists do not contain duplicates.
    What does this mean for your testing procedure?

    Provide an ordered list of properties by strength using the weakear and stronger definitions.
    Can you automate the test process? Use the techniques presented in this week's lecture. Also use QuickCheck.
    Deliverables: Haskell program, concise test report, indication of time spent.

-}


{- 
    Remove the first element of the list x & the first occurrence of that same element in the list y.
    Return the lists after looping over all elements of list x.
    http://zvon.org/other/haskell/Outputlist/delete_f.html
-}
removePairs :: Eq a => ([a], [a]) -> ([a], [a])
removePairs ([], ys) = ([], ys)
removePairs (xs, []) = (xs, [])
removePairs ((x:xs), ys)    | (length ((delete x ys)) == (length ys)) = ([x],[])
                            | otherwise = removePairs (xs, (delete x ys))

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys = ((length x) == 0) && ((length y) == 0)
                        where (x,y) = removePairs (xs, ys)

{-
    TO DO:
    Next, define some testable properties for this function,
    and use a number of well-chosen lists to test isPermutation.
    You may assume that your input lists do not contain duplicates.
    What does this mean for your testing procedure?
    Provide an ordered list of properties by strength
    using the weakear and stronger definitions. 
    Can you automate the test process?
    Use the techniques presented in this week's lecture. Also use QuickCheck.
    Deliverables: Haskell program, concise test report, indication of time spent.
-}


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
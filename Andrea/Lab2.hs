
module Lab2 where
import Data.List
import Data.Char
import System.Random
import Test.QuickCheck

infix 1 --> 

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

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

 testNoTriangles1, testNoTriangles2, testNoTriangles3 :: Integer -> Integer -> Integer -> Bool
 testNoTriangles1 a b c = (a > b + c) --> triangle a b c == NoTriangle
 testNoTriangles2 a b c = (b > c + a) --> triangle a b c == NoTriangle
 testNoTriangles3 a b c = (c > a + b) --> triangle a b c == NoTriangle
 
 testIsosceles1, testIsosceles2, testIsosceles3 :: Integer -> Integer -> Bool
 testIsosceles1 a b = (a > 0 && b > 0 && a /= b) --> triangle a a b == Isosceles
 testIsosceles2 a b = (a > 0 && b > 0 && a /= b) --> triangle b a a == Isosceles
 testIsosceles3 a b = (a > 0 && b > 0 && a /= b) --> triangle a b a == Isosceles
 
 testRectangular1, testRectangular2, testRectangular3 :: Integer -> Integer -> Integer -> Bool
 testRectangular1 a b c = (a > 0 && b > 0 && c > 0 && a^2 + b^2 == c^2) 
                         --> triangle a b c == Rectangular
 testRectangular2 a b c = (a > 0 && b > 0 && c > 0 && b^2 + c^2 == a^2) 
                         --> triangle a b c == Rectangular
 testRectangular3 a b c = (a > 0 && b > 0 && c > 0 && a^2 + c^2 == b^2) 
                         --> triangle a b c == Rectangular
 
 testEquilateral :: Integer -> Bool
 testEquilateral a = (a > 0) --> triangle a a a == Equilateral

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

removePairs :: Eq a => ([a], [a]) -> ([a], [a])
removePairs ([], ys) = ([], ys)
removePairs (xs, []) = (xs, [])
removePairs ((x:xs), ys)    | (length ((delete x ys)) == (length ys)) = ([x],[])
                            | otherwise = removePairs (xs, (delete x ys))

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys = ((length x) == 0) && ((length y) == 0)
                        where (x,y) = removePairs (xs, ys)
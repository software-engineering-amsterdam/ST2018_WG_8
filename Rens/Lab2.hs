module Lab2 where

import Data.List
import Data.Char
import System.Random
import Test.QuickCheck
import Control.Monad

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
    a <- length <$> filter (\x -> x <= 0.250000000000000000) <$> floaties
    print(a)
    b <- length <$> filter (\x -> x <= 0.500000000000000000 && x > 0.250000000000000000) <$> floaties
    print(b)
    c <- length <$> filter (\x -> x <= 0.750000000000000000 && x > 0.500000000000000000) <$> floaties
    print(c)
    d <- length <$> filter (\x -> x <= 1.000000000000000000 && x > 0.750000000000000000) <$> floaties
    print(d)
    putStrLn("Sum of the quantile contents:" )
    print(a+b+c+d)
        where floaties = (probs 10000)

-- Sum of the quantiles does not add up to 10k. No matter the amount of 0's.
-- Although the distribution seems quite random but around 2500, the best way
-- to provide proof is by statistics.

-- Exercise 2
whatShape :: (Int, Int, Int) -> Shape
whatShape (x, y, z)
    | isNoTriang (x, y, z)  = NoTriangle
    | x == y && y == z = Equilateral
    | x == y || z == y || z == x = Isosceles
    | isRect (x, y, z) = Rectangular
    | otherwise = Other

isRect :: (Int, Int, Int) -> Bool
isRect (x, y, z) = ((x ^ 2) + (y ^ 2) == z ^ 2) || ((x ^ 2) + (z ^ 2) == y ^ 2) || ((z ^ 2) + (y ^ 2) == x ^ 2)

isNoTriang :: (Int, Int, Int) -> Bool
isNoTriang (x, y, z) = not (x + y > z && x + z > y && z + y > x)

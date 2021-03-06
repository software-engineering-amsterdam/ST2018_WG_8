module Lab1 where
import Data.List
import Test.QuickCheck
import Testdata

prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
    where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]
infix 1 -->

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

reversal :: Integer -> Integer
reversal = read . reverse . show

-- Exercise 1 (30 minutes)

-- Rewritten functions for calulating the sum of
-- squared numbers in a list and through formula
sumSquaring :: [Integer] -> Integer
sumSquaring [] = 0
sumSquaring (x:xs) = (x ^ 2) + sumSquaring xs

sumSquared :: Integer -> Integer
sumSquared n = sumSquaring [0..n]

sumSquared' :: Integer -> Integer
sumSquared' n = (n * (n + 1) * ((2 * n) + 1)) `div` 6

-- Check the squaring function (match inputs)
checkSquaring :: Integer -> Bool
checkSquaring n = (n >= 0) --> sumSquared n == sumSquared' n

-- Do the same thing for the power of three formula and sum.
sumTripsList :: [Integer] -> Integer
sumTripsList [] = 0
sumTripsList (x:xs) = (x ^ 3) + sumTripsList xs

sumTrips :: Integer -> Integer
sumTrips n = sumTripsList [0..n]

sumTrips' :: Integer -> Integer
sumTrips' n = (n * (n + 1) `div` 2) ^ 2

-- Check the trips function (match inputs)
checkTrips :: Integer -> Bool
checkTrips n = (n >= 0) --> sumTrips n == sumTrips' n

-- Exercise 2 (20 minutes)
-- This is much harder to check because the amount of subsequences generated from
-- 55 and onward are monumental. (36028797018963968 subsequences)
-- A test like this can never prove the validity of the proposition, mathmatical
-- validation is simply not feasible. In this test we simply check if the first 100
-- Integers are coherent with our theory. If we want proper proof, we need induction.
checkSubSeqCardinality :: Integer -> Bool
checkSubSeqCardinality n = (n >= 0) --> (length (subsequences [1..n]) == 2 ^ n)

-- Exercise 3 (30 minutes)
-- Intuitively the amount of permutations in list 1..n is n factorial, so first
-- I define a simple recursive factorial function.

factorial' :: Int -> Int
factorial' 0 = 1
factorial' 1 = 1
factorial' n = n * (factorial' (n - 1))

-- Check if the amount of perms in a list of n elems is equal to n factorial.
checkPermutations :: Int -> Bool
checkPermutations n = (n >= 0) --> (length (permutations [1..n]) == factorial' n)
-- Similar to the testing of the subsequence cardinality versus 2 ^ n
-- this only proves the mathmatical property for a small portion (1..12) of
-- the entire scope (all natural numbers). So once again we need induction to
-- deliver any usefull prove.

-- Exercise 4 (40 minutes)
primesTo10000 :: [Integer]
primesTo10000 = filter prime [1..10000]

generateReversePrimes :: [Integer]
generateReversePrimes = filter isReversable primesTo10000
    where isReversable n = elem (reversal n) primesTo10000

-- Due to the constraint of 10000 numbers the best and most thorough way is to
-- simply write down all the reversables and check them against the generated
-- list.

-- Exercise 5 (20 minutes)
sumOf101Primes :: Int -> [Integer]
sumOf101Primes n = sum( drop n (take (101 + n) primes)) : sumOf101Primes (n + 1)

smallest101PrimeSum :: Integer
smallest101PrimeSum = head (filter prime (sumOf101Primes 0))

-- I am pretty sure my answer is correct, I could easily check it by manually
-- checking the prime'ness of each smaller summed number or go as far as calculate
-- the sum of all lower 101 sequences of prime numbers in order to prove the correctness
-- of the funciton.

-- Exercise 6 (20 minutes)
-- Simple function that calculates n prime multiplications plus 1.
primeMults :: [Int] -> [Integer]
primeMults [] = []
primeMults (n:ns) = product (take n primes) + 1 : primeMults ns

-- Generate a list of primeMults and filter them until n non-primes are found.
conjenctures :: Int -> [Integer]
conjenctures n = take n (filter (not . prime) (primeMults [1..]))

-- This generates the first answer of 6, the first six primes product up to 30031, +1
-- makes 30031, which is not a prime number.

-- Exercise 7 (2 hours)
-- First, double every other ditgit starting from the rightmost digit.
doubleEveryOther :: [Int] -> [Int]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = [x]
doubleEveryOther (x:y:z) = x : (y * 2) : doubleEveryOther z

-- Now subtract 9 from each number greater than 9 as a result of the operation.
reduceLargeNum :: Int -> Int
reduceLargeNum x
    | x > 9     = x - 9
    | otherwise = x

sumLargeNums :: [Int] -> [Int]
sumLargeNums [] = []
sumLargeNums (x:xs) = reduceLargeNum x : sumLargeNums xs

luhn :: Integer -> Bool
luhn x = (sum(convertedNum) `mod` 10) == 0
    where
        convertedNum = sumLargeNums (doubleEveryOther (reverse (digits x)))

digits :: Integer -> [Int]
digits n = map (\x -> read [x] :: Int) (show n)

-- AmEx is 15 digits long and starts with 34 or 37 (should be greater than 36 due to necessicty for 2 digits)
isAmericanExpress, isMaster, isVisa :: Integer -> Bool
isAmericanExpress x = (x > 37) && ((firstTwo == [3,4] || firstTwo == [3,7]) && length (digits x) == 15 && luhn x)
    where firstTwo = take 2 (digits x)

-- MasterCard is 16 digits long and starts with 5 followed by 1-5
isMaster x = (x > 50) && (firstDigitCorrect && secondDigitCorrect && (length (digits x) == 16) && luhn x)
    where
        firstDigitCorrect = head (digits x) == 5
        secondDigitCorrect = (digits x) !! 1 < 6 && (digits x) !! 1 > 0

-- Visa is 16 digits long and starts with 4
isVisa x = head (digits x) == 4 && length (digits x) == 16 && luhn x

-- Now create a couple of test cases for each bank, knowing they are right:
-- The problem is that you don't have another formula to check against.
-- We will have to do with a lot of numbers that are sure to be right (online source).
-- Moreover we can test if cc numbers only belong to one bank max (which should hold).

-- This function takes one of the isMaster/isVisa/isAmericanExpress funcs and a list of cc nums.
testCards :: (Integer -> Bool) -> [Integer] -> Bool
testCards func [] = True
testCards func (x:xs) = (func x) && testCards func xs

-- Test if a creditcard number fits any of the three banks (if so, it shouldn't fit the others)
testCreditcards :: Integer -> Bool
testCreditcards n = (n >= 0) --> not ((isVisa n && isMaster n) || (isMaster n && isAmericanExpress n) || (isAmericanExpress n && isVisa n))

-- A far more reliable test would be to combine QuickCheck functionality with a certified CC number generator and checker.
-- It would have to accept any number generated by the CC generator, deny all cases denied by the checker.
-- Moreover, the testCreditcards function with QuickCheck tests some edge cases (-1, 0, 12) which would never
-- be an accepted CC number. This was good because it showed me when I tried that my functions failed for some
-- edge cases.

-- Exercise 8 (1.5 hours)

data Boy = Matthew | Peter | Jack | Arnold | Carl
    deriving (Eq,Show)
boys = [Matthew, Peter, Jack, Arnold, Carl]

-- The accusation function result is derived by processing the boys' statements
accuses :: Boy -> Boy -> Bool
accuses x y = (getBoy x) y

-- Change a boys name into the appropriate function application
getBoy :: Boy -> (Boy -> Bool)
getBoy y
    | y == Matthew = matthew
    | y == Peter = peter
    | y == Jack = jack
    | y == Arnold = arnold
    | y == Carl = carl

-- Find the accusers/accused boys/boy by boys/boy by application of the accuses function.
-- This function logically derives a True or False from processing the statements.
accusers :: Boy -> [Boy]
accusers boy = [p | p <- boys, accuses p boy]

accused :: Boy -> [Boy]
accused boy = [p | p <- boys, accuses boy p]

-- Define the logic of the statements of each boys, use Boy's for direct accusations
-- use the functions for statements about the other statements.
matthew = \x -> not (x == Matthew) && not (x == Carl)
peter   = \x -> x == Matthew || x == Jack
jack    = \x -> not (matthew x) && not (peter x)
arnold  = \x -> matthew x /= peter x
carl    = \x -> not (arnold x)

-- All subsequences of 3 boys, the max amount of boys that can speak the truth.
possibleTruthers :: [[Boy]]
possibleTruthers = [p | p <- combinations, (length p) == 3]
    where combinations = subsequences boys

-- Check whether all boys in a group of three point to the same culprit.
-- If so, these boys must be speaking the truth as only one culprit can exist
-- within the boundaries of this logic.
findSingleCulprit :: [Boy] -> [Boy]
findSingleCulprit (x:y:z:zs) = (intersect (intersect (accused x) (accused y)) (accused z))

-- Find the culprit by looking for a set of 3 boys, assuming they are speaking
-- the truth, that all accuse the same boy.
findCulprit :: [[Boy]] -> [Boy]
findCulprit [] = []
findCulprit (x:xs)
    | (findSingleCulprit x) /= [] = (findSingleCulprit x)
    | otherwise = (findCulprit xs)

-- Generate the guilty boy by finding the culprit amongst all possible truthers,
-- find the honest ones by rerunning the function to find who are accusing the
-- guilty party and find the liars as a difference between the honest boys and all boys.
guilty, honest, liars :: [Boy]
guilty = (findCulprit possibleTruthers)
honest = accusers (head guilty)
liars = boys \\ honest

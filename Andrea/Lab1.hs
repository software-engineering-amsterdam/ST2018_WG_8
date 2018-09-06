module Lab1 where
import Data.List
import Test.QuickCheck  

-- USED 
prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
  where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..] 

reversal :: Integer -> Integer
reversal = read . reverse . show

divide :: Integer -> Integer -> Bool
divide n m = rem m n == 0

isPrime :: Integer -> Bool
isPrime n = n > 1 && all (\ d -> not (divide d n)) [2..n-1]

-- NOT USED YET

infix 1 --> 
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

data Boy = Matthew | Peter | Jack | Arnold | Carl 
           deriving (Eq,Show)

boys = [Matthew, Peter, Jack, Arnold, Carl]

{- 
    1. Redo exercises 2 & 3 of workshop 1 by writing quickcheck tests
    Sum of n^2 = (n(n + 1)(2n + 1)) / 6
    Sum of n^3 = ((n(n+ 1))/2)^2
-}

sumSquared :: Integer -> Integer
sumSquared n = (n * (n + 1) * (2 * n + 1)) `div` 6

sumSquaredInd :: Integer -> Integer
sumSquaredInd 1 = 1
sumSquaredInd n = n ^ 2 + sumSquared(n - 1)

sumTripleSquared :: Integer -> Integer
sumTripleSquared n = ( (n * (n + 1)) `div` 2) ^ 2

sumTripleSquaredInd :: Integer -> Integer
sumTripleSquaredInd 1 = 1
sumTripleSquaredInd n = n ^ 3 + sumTripleSquaredInd(n - 1)

squaredTest :: Integer -> Bool
squaredTest n = sumSquared n == sumSquaredInd n

tripledTest :: Integer -> Bool
tripledTest n = sumTripleSquared (abs n) == sumTripleSquaredInd (abs n)
-- HAS TO BE ABSOLUTE
--  1 HOUR

{-
    2. 
    Prove by induction that if A is a finite list with |A| = n , then
    P(A) = 2 ^ n, and testing the property for integer lists of the form 
    [1..n].
-}

-- HAS TO BE ABSOLUTE 

powerCard :: [Integer] -> Int
powerCard n = length (subsequences n)

powerCardPower :: [Integer] -> Int
powerCardPower n = 2 ^ (length n)

powerCardInd :: [Integer] -> Int
powerCardInd [] = 1
powerCardInd (x:xs) = 2 * powerCardInd xs

powerCardTest :: [Integer] -> Bool
powerCardTest n = if powerCard n == powerCardPower n 
                    then powerCard n == powerCardInd n
                    else False
-- DOUBLE?

-- 30 MINS

{- 
    3. Redo exercise 5 of Workshop 1 by replacing sets by lists,
    and testing the property for integer lists of the form [1..n].

    Is the property hard to test? If you find that it is, can you given a reason why?
    Again, give your thoughts on the following issue:
    when you perform the test for exercise 5,
    what are you testing actually? 
    Are you checking a mathematical fact? 
    Or are you testing whether perms satisfies a part of its specification? 
    Or are you testing something else still?
-}

-- HAS TO BE ABSOLUTE 
perms :: [Integer] -> Int
perms [] = 1
perms (x:xs) = length (x:xs) * perms xs

permsTest :: [Integer] -> Bool
permsTest n = perms n == length (permutations n)

-- 10 MINS

{- 
    4. The natural number 13 has the property that it is prime and its reversal,
    the number 31, is also prime. Write a function that finds all primes < 10000
    with this property. How would you test this function, by the way?
-}

reversalPrimes :: [Integer]
reversalPrimes = filter prime (map reversal xs)
    where xs = takeWhile (< 10000) primes

-- 20 MINS

-- Just check 

{-
    5. The number 101 is a prime, and it is also the sum of five consecutive primes,
    namely 13+17+19+23+29. Find the smallest prime number that is a sum of 101
    consecutive primes.
    Do you have to test that your answer is correct? How could this be checked?
-}

-- genSums :: Int -> [Integer]
-- genSums = sum (take 101 (drop n primes)) 

-- genPrime :: Integer
-- genPrime =  head filter isPrime (genNumber head x, [ x | x <- [0..] ])
--     then n else tail x


-- If no prime, then tail and redo

{-
    6. Using Haskell to refute a conjecture.
    Write a Haskell function that can be used to refute the
    following conjecture. "If p1,...,pn is a list of consecutive primes
    starting from 2, then (p1×⋯×pn)+1 is also prime." 
    This can be refuted by means of a counterexample,
    so your Haskell program should generate counterexamples.
    What is the smallest counterexample?
-}

conjectures :: Int
conjectures = head [(product (take x primes) + 1) | x <- [1..], not (prime [product (take x primes)]) ]



{-
    Implement and test the Luhn Algorithm. Finally,
    design and implement a test for correctness of your implementation.
-}

-- luhn :: Integer -> Bool

-- isAmericanExpress, isMaster, isVisa :: Integer -> Bool

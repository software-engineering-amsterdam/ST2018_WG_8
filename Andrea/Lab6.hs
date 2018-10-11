module Lab6 
    where
import Data.List
import System.Random
import Lecture6

{- 
    Exercise 1:
    Implement a function exM.
    It should do modular exponentiation of x^y in polynomial time, by repeatedly squaring modulo N.
    Used explanation of: https://www.khanacademy.org/computing/computer-science/cryptography/modarithmetic/a/fast-modular-exponentiation
-}

exM' :: Integer -> Integer -> Integer -> Integer
exM' a b c = modMul c (modPower a c ( (toPower (toBin b))))

-- Calculates the mod C of the powers of two, <= B.
modPower :: Integer -> Integer -> [Integer] -> [Integer]
modPower a c [] = []
modPower a c (x:xs) = a ^ x `mod` c : modPower a c xs

-- Returns the product of the list of powers generated by modPower.
modMul ::  Integer -> [Integer] -> Integer
modMul c lst = (product lst) `mod` c

-- Decimal to reversed Binary number, found on The Internet.
-- https://stackoverflow.com/questions/9166148/how-to-implement-decimal-to-binary-function-in-haskell
toBin :: Integer -> [Integer]
toBin 0 = []
toBin n = let (q,r) = n `divMod` 2 in r : toBin q

-- Transform all elements in list to 2 ^ index x, if x is non-zero.
-- E.g.  [1, 1, 1, 1, 0, 1] -> [1, 2, 4, 8, 0, 32]
toPower :: [Integer] -> [Integer]
toPower [] = []
toPower (x:xs) = x : toPower (map (2 *) xs)

{-
    Exercise 2:
    Check that your implementation is more efficient than expM by running a number of relevant tests and documenting the results.
-}




{- 
    Exercise 3: 
    In order to test Fermat's Primality Check (as implemented in function prime_test_F),
    the list of prime numbers generated by Eratosthenes' sieve is useless,
    for Fermat's Primality Check correctly classify the primes as primes.
    Where the check can go wrong is on classifying composite numbers; these can slip through the Fermat test.
    Write a function composites :: [Integer] that generates the infinite list of composite natural numbers.
-}

{- 
    Exercise 4:
    Use the list of composite numbers to test Fermat's primality check.
    What is the least composite number that you can find that fools the check, for prime_tests_F k with k = 1, 2, 3?
    What happens if you increase k?
-}


{- 
    Exercise 5: 
    Use the list generated by the following function for a further test of Fermat's primality check.
    Read the entry on Carmichael numbers on Wikipedia to explain what you find.
    If necessary, consult other sources.
-}

carmichael :: [Integer]
carmichael = [ (6 * k + 1) * (12 * k + 1) * (18 * k + 1) |
    k <- [2..], 
    prime (6 * k + 1), 
    prime (12 * k + 1), 
    prime (18 * k + 1) ]

{- 
    Exercise 6:
    Use the list from the previous exercise to test the Miller-Rabin primality check. What do you find?
-}

{- 
    Exercise 6.2:
    You can use the Miller-Rabin primality check to discover some large Mersenne primes. 
    The recipe: take a prime p, and use the Miller-Rabin algorithm to check whether 2^p−1 is also prime.
    Find information about Mersenne primes on internet and check whether the numbers that you found are genuine Mersenne primes. Report on your findings.
-}

{- 
    Exercise 7 (Bonus):
    For RSA public key cryptography, one needs pairs of large primes with the same bitlength.
    Such pairs of primes can be found by trial-and-error using the Miller-Rabin primality check.
    Write a function for this, and demonstrate how a pair p,q that you found can be used for public key encoding and decoding.
-}
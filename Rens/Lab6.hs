module Lab6
    where
import Data.List
import Data.Bits
import System.Random
import Lecture6
import System.TimeIt



{-
    Exercise 1:
    Implement a function exM.
        exM :: Integer -> Integer -> Integer -> Integer
    that does modular exponentiation of xy in polynomial time, by repeatedly squaring modulo N.
    E.g., x^33 mod 5 can be computed by means of
        x^33 (mod 5) = x^ 32 (mod 5) × x (mod 5).
        x^32 (mod N) = x^2(mod N) → x^4 (mod N)→ … → x^32 (mod N).
-}




{-
    Exercise 2:
    Check that your implementation is more efficient than expM by running a number of relevant tests and documenting the results.
-}

randomLargeNumber :: IO Integer
randomLargeNumber = randomRIO (1000000,10000000)

-- Time the efficiency of exM function with inputs of several large numbers.
-- Compare time to the given expM function. This will run N times.
testExMEff :: Integer -> IO()
testExMEff 0 = do
    (putStrLn "Done!")
testExMEff n = do
    a <- randomLargeNumber
    b <- randomLargeNumber
    m <- randomLargeNumber

    -- Run the experiment for both functions and print the result and time.
    putStrLn ("\nStart expM with\na: " ++ (show a) ++ "\nb: " ++ (show b) ++"\nm: " ++ (show m) ++ "\n")
    timeIt $ putStrLn ("Result: " ++ show (expM a b m))

    putStrLn "\nStart exM with the same numbers."
    timeIt $ putStrLn ("Result: " ++ show (exM a b m))
    rest <- (testExMEff (n - 1))
    return rest

{-
    Exercise 3:
    In order to test Fermat's Primality Check (as implemented in function prime_test_F),
    the list of prime numbers generated by Eratosthenes' sieve is useless,
    for Fermat's Primality Check correctly classify the primes as primes.
    Where the check can go wrong is on classifying composite numbers; these can slip through the Fermat test.
    Write a function composites :: [Integer] that generates the infinite list of composite natural numbers.
-}

composites :: [Integer]
composites = [x | x <- [1..], x > 1, not (prime x)]

{-
    Exercise 4:
    Use the list of composite numbers to test Fermat's primality check.
    What is the least composite number that you can find that fools the check, for prime_tests_F k with k=1,2,3 ?
    What happens if you increase k?
-}

-- I found 9 to be the lowest number to fool the test. I used the results form
-- k 1, 2 and 3 and checked if either of them gave a false positive on the
-- elements form the list of composites. However, if the functions don't give
-- false positives it can run for long times and reach very high numbers of
-- composites. This can take a long time while the first actual fooling numbers are
-- low in the first 100. Therefore I loop the function between 1 and 100 in order
-- to get a fooling number under the 100. I noticed with higher values for k,
-- it will less often give a false positive but it will also run longer because
-- it will take more time to find the right number.
-- With 0 it will always return a false positive.

foolcomposites = [(primeTestsF 1 r, primeTestsF 2 r, primeTestsF 3 r) | r <- composites]

foolTest :: Int -> IO Integer
foolTest 100 = foolTest 0
foolTest n = do
    let r = foolcomposites !! n
    r1 <- fst' r
    r2 <- snd' r
    r3 <- thrd r
    if r1 || r2 || r3 then return (composites !! n) else ((foolTest (n + 1)))

thrd :: (IO Bool, IO Bool, IO Bool) -> IO Bool
thrd (x, y, z) = z

snd' :: (IO Bool, IO Bool, IO Bool) -> IO Bool
snd' (x, y, z) = y

fst' :: (IO Bool, IO Bool, IO Bool) -> IO Bool
fst' (x, y, z) = x


{-
    Exercise 5:
    Use the list generated by the following function for a further test of Fermat's primality check.

    > carmichael :: [Integer]
    > carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) |
    >       k <- [2..],
    >       prime (6*k+1),
    >       prime (12*k+1),
    >       prime (18*k+1) ]

    Read the entry on Carmichael numbers on Wikipedia to explain what you find. If necessary, consult other sources.
-}

carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) |
   k <- [2..],
   prime (6*k+1),
   prime (12*k+1),
   prime (18*k+1) ]

-- Carmichael's numbers are composites, by feeding these numbers to the primeTestsF
-- function and checking when it returns true we can see what numbers fool the test.
foolcarmichael = [(primeTestsF 1 r, primeTestsF 2 r, primeTestsF 3 r) | r <- carmichael]

foolCTest :: Int -> IO Integer
foolCTest 100 = foolTest 0
foolCTest n = do
    let r = foolcarmichael !! n
    r1 <- fst' r
    r2 <- snd' r
    r3 <- thrd r
    if r1 || r2 || r3 then return (carmichael !! n) else ((foolCTest (n + 1)))

-- Need some explaination on the high fail rate in carmichael numbers on the fermat test.


{-
    Exercise 6:
    Use the list from the previous exercise to test the Miller-Rabin primality check. What do you find?
-}

foolMRcarmichael = [(primeMR 1 r, primeMR 2 r, primeMR 3 r) | r <- carmichael]

foolMRTest :: Int -> IO Integer
foolMRTest 100 = foolTest 0
foolMRTest n = do
    let r = foolMRcarmichael !! n
    r1 <- fst' r
    r2 <- snd' r
    r3 <- thrd r
    if r1 || r2 || r3 then return (carmichael !! n) else ((foolMRTest (n + 1)))

-- The primeMR test also fails but much less stable. Explain why:

{-
    Exercise 6.2:
    You can use the Miller-Rabin primality check to discover some large Mersenne primes.
    The recipe: take a prime p, and use the Miller-Rabin algorithm to check whether 2^p−1 is also prime.
    Find information about Mersenne primes on internet and check whether the numbers that you found are genuine Mersenne primes. Report on your findings.
-}

-- Takes a starting integer (as a power of 2) of at least 2 and an empty list to fill
-- and appends every primeMR to the list.
mersenneMRPrime :: Integer -> [Integer] -> IO [Integer]
mersenneMRPrime 0 _ = do mersenneMRPrime 2 []
mersenneMRPrime 1 _ = do mersenneMRPrime 2 []
mersenneMRPrime n l = do
    let powerof2 = 2 ^ n
    x <- primeMR 1 (powerof2 - 1)
    if x then (mersenneMRPrime (n + 1) (l ++ [(powerof2 - 1)])) else (mersenneMRPrime (n + 1) l)

-- Generates a list of possible mersenne primes. Use (take x) <$> mersenneMRPrime 0 []

{-
    Exercise 7 (Bonus):
    For RSA public key cryptography, one needs pairs of large primes with the same bitlength.
    Such pairs of primes can be found by trial-and-error using the Miller-Rabin primality check.
    Write a function for this, and demonstrate how a pair p,q that you found can be used for public key encoding and decoding.
-}

main :: IO()
main = do
    testExMEff 3

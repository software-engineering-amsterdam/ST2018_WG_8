module ExamFuncs
    where
import Data.List
import Test.QuickCheck
import Data.Char
import Data.Tuple
import SetOrd
import System.Process
import System.Random
import Control.Monad
import ExamHelpers
import Data.Bits

{-
    Prime functions (tests and algorithms) and gcd

-}

-- Regular prime test.
prime :: Integer -> Bool
prime n = n > 1 && all (\ x -> rem n x /= 0) xs
    where xs = takeWhile (\ y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]

-- Fermats prime test.
primeTestsF :: Int -> Integer -> IO Bool
primeTestsF k n = do
 as <- sequence $ fmap (\_-> randomRIO (2,n-1)) [1..k]
 return (all (\ a -> exM a (n-1) n == 1) as)

-- Miller Rabin prime test.
primeMR :: Int -> Integer -> IO Bool
primeMR _ 2 = return True
primeMR 0 _ = return True
primeMR k n = do
    a <- randomRIO (2, n-1) :: IO Integer
    if exM a (n-1) n /= 1 || mrComposite a n
    then return False else primeMR (k-1) n

-- One number in between that is prime, or gcd of 1.
coprime :: Integer -> Integer -> Bool
coprime n m = fGcd n m == 1

coprimes :: [(Integer,Integer)]
coprimes = filter (uncurry coprime) pairs

-- All non-primes
composites :: [Integer]
composites = [ x | x <- [2..], not (prime x)]

-- Miller rabin composite
mrComposite :: Integer -> Integer -> Bool
mrComposite x n = let
    (r,s) = decomp (n-1)
    fs     = takeWhile (/= 1)
       (map (\ j -> exM x (2^j*s) n)  [0..r])
  in
    exM x s n /= 1 && last fs /= (n-1)

-- Produces the two numbers that make up a given input number after multiplication.
factors :: Integer -> [Integer]
factors n0 = let
   ps0 = takeWhile (\ m -> m^2 <= n0) primes
 in factors' n0 ps0 where
   factors' 1 _  = []
   factors' n [] = [n]
   factors' n (p:ps)
    | n `mod` p == 0 = p: factors' (n `div` p) (p:ps)
    | otherwise      =    factors' n ps

fctGcd :: Integer -> Integer -> (Integer,Integer)
fctGcd a b =
  if b == 0
  then (1,0)
  else
     let
       (q,r) = quotRem a b
       (s,t) = fctGcd b r
     in (t, s - q*t)

-- Carmichael numbers
carmichael :: [Integer]
carmichael = [ (6*k+1)*(12*k+1)*(18*k+1) |
    k <- [2..],
    prime (6*k+1),
    prime (12*k+1),
    prime (18*k+1) ]

-- Mersenne primes
mers :: Integer -> Integer
mers 1  = 2^2-1;    mers 2  = 2^3-1;     mers 3  = 2^5-1
mers 4  = 2^7-1;    mers 5  = 2^13-1;    mers 6  = 2^17-1
mers 7  = 2^19-1;   mers 8  = 2^31-1;    mers 9  = 2^61-1
mers 10 = 2^89-1;   mers 11 = 2^107-1;   mers 12 = 2^127-1
mers 13 = 2^521-1;  mers 14 = 2^607-1;   mers 15 = 2^1279-1
mers 16 = 2^2203-1; mers 17 = 2^2281-1;  mers 18 = 2^3217-1
mers 19 = 2^4253-1; mers 20 = 2^4423-1;  mers 21 = 2^9689-1
mers 22 = 2^9941-1; mers 23 = 2^11213-1; mers 24 = 2^19937-1
mers 25 = 2^21701-1;
mers _  = undefined

-- Quick calculation for (a ^ b) mod m
exM :: Integer -> Integer -> Integer -> Integer
exM a 0 m = 1
exM a b m = t * exM ((a * a) `mod` m) (shiftR b 1) m `mod` m
    where t = if testBit b 0 then a `mod` m else 1

{-
    Properties
    Use domains such as [-10..10]
-}

-- For properties over a certain domain xs
stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\ x -> p x --> q x)
weaker   xs p q = stronger xs q p

validHoare :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
validHoare xs p q = stronger xs p q

{-
    Logic forms

    Works with forms of∷
    form1 = Equiv (Impl p q) (Impl (Neg q) (Neg p))
    form2 = Equiv (Impl p q) (Impl (Neg p) (Neg q))
    form3 = Impl (Cnj [Impl p q, Impl q r]) (Impl p r)

-}

-- Check if the function is a Tautology by using allVals on the evl of two funcs.
tautology :: Form -> Bool
tautology f = all (\ v -> evl v f) (allVals f)

-- Use an inversion of the satisfiable as the contradicion function.
contradiction :: Form -> Bool
contradiction f = not (satisfiable f)

-- Logical entailment: B logically entails A is true if and only if
-- all of the truth valuations that satisfy B also satisfy A.
entails :: Form-> Form -> Bool
entails f g = tautology (Impl f g)

-- Equivalence checks if two forms imply eachother and uses tautology to check
-- if this goes for all inputs.
equivalence :: Form -> Form -> Bool
equivalence f g = tautology (Equiv f g)

satisfiable :: Form -> Bool
satisfiable f = any (\ v -> evl v f) (allVals f)

-- Use this for conversion to cnf.
cnf :: Form -> Form
cnf frm = while (not . isCnf) strictCnf inp
      where inp = toPairs (nnf (arrowfree frm))

optimizedCnf :: Form -> Form
optimizedCnf frm = optimize (cnf frm)


{-
    Set functions and relationship closures.

-}

-- Checks if something is a permutation
isPermutation :: Ord a => [a] -> [a] -> Bool
isPermutation list1 list2 = compareLists sorted1 sorted2
    where
        sorted1 = sort(list1)
        sorted2 = sort(list2)

-- Checks if something is a derangement.
isDerangement :: Ord a => [a] -> [a] -> Bool
isDerangement list1 list2 = (isPermutation list1 list2) && (checkNotEq list1 list2)

intersectionSet :: Ord a => Set a -> Set a -> Set a
intersectionSet (Set xs) set2 = Set ([x | x <- xs, inSet x set2])

differenceSet :: Ord a => Set a -> Set a -> Set a
differenceSet (Set xs) (Set ys) = Set (sort((xs \\ ys) ++ (ys \\ xs)))

unionSet :: (Ord a) => Set a -> Set a -> Set a
unionSet (Set xs) (Set ys) = Set (sort (nub (xs ++ ys)))

powerSet :: Ord a => Set a -> Set (Set a)
powerSet (Set xs) =
   Set (sort (map (\xs -> (list2set xs)) (powerList xs)))

-- If (x,y) ∈ A then (y,x) ∈ A
symClos :: Ord a => Rel a -> Rel a
symClos xs = sort(xs ++ [(swap x) | x <- xs, not ((swap x) `elem` xs)])

-- If (x,y) ∈ A ^ (y,z) ∈ A -> (x,z) ∈ A
trClos :: Ord a => Rel a -> Rel a
trClos = fp (\xs -> sort (nub (xs ++ xs @@ xs)))

-- ∀ x, y ∈ A, xRy ∨ yRx ∨ x=y
isLinear :: Eq a => [a] -> Rel a -> Bool
isLinear dom rels = all (== True) [elem (a, b) rels || elem (b, a) rels || a == b | a <- dom, b <- dom]

jos n k 0 = rem (k-1) n
jos n k i = rem (k + jos (n-1) k (i-1)) n

meeny :: Int->Int->[String]->String
meeny 0 _ _ = "Empty k"
meeny _ 0 _ = "Invalid round"
meeny k i s = s !! abs(jos (length(s)) k (i - 1))

probWin1 n i p = let q = 1 - p in
    p^(n-i) * sum [ p^j * q^(i-j-1) | j <- [0..i-1] ] / sum [ p^j * q^(n-j-1) | j <- [0..n-1] ]

probWin2 n i p = let r = (1-p)/p in
    sum [ r^j | j <- [0..i-1] ] / sum [ r^j | j <- [0..n-1]]

probWin3 n i p = let q = 1 - p in
    (1 - (q/p)^i)/(1-(q/p)^n)

data Tree a = T a [Tree a] deriving (Eq,Ord,Show)

-- Tree consists of (1,1) which are coprimes
-- Any subsequent node consists of any a, b that are the pair of a coprime part
-- a and a + b or b and b + a. as the gcd is found by removing a from b or vice versa.
-- via the euclidian algorithm (and then reversing etc) the adding of a to b only
-- adds an extra step to this algorithm, having to remove an extra a. The result
-- is still the same! The gcd is 1, the numbers are coprime.

grow :: (node -> [node]) -> node -> Tree node
grow step seed = T seed (map (grow step) (step seed))

tree1 n = grow (step1 n) (1,1)
step1 n = \ (x,y) -> if x+y <= n then [(x+y,x),(x,x+y)] else [] -- step function

tree2 n = grow (step2 n) (1,1)
step2 n = \ (x,y) -> if x+y <= n then [(x+y,y),(x,x+y)] else [] -- step function

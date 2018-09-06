
module Lab1 where
import Data.List
import Test.QuickCheck
data Nat = Zero | Succ Nat 

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

data Boy = Matthew | Peter | Jack | Arnold | Carl 
           deriving (Eq,Show)

boys = [Matthew, Peter, Jack, Arnold, Carl]

----1a
sumSquareList :: [Int]->Int
sumSquareList [] = 0
sumSquareList (x:xs) = (x^2) + sumSquareList xs

sumSquare :: Int->Int
sumSquare n = sumSquareList [0..n] 

sumSquare' :: Int -> Int
sumSquare' n = (n*(n+1)*(2*n+1)) `div` 6 

testSumSquare :: Int ->Bool
testSumSquare x  = sumSquare (abs x) == sumSquare' (abs x)

----1b

sumThreeList :: [Int]->Int
sumThreeList [] = 0
sumThreeList (x:xs) = (x^3) + sumThreeList xs

sumThree :: Int->Int
sumThree n = sumThreeList [1..n]

sumThree' :: Int -> Int
sumThree' n = (n*(n+1) `div` 2)^2 

testThreeSquare :: Int ->Bool
testThreeSquare x  = sumThree (abs x) == sumThree' (abs x)

----2
subListLength :: Int->Int
subListLength n = length(subsequences [1..(abs n)])

powerN :: Int->Int
powerN n = (2^n)

testPowerset :: Int ->Bool
testPowerset x  = subListLength (abs x) == powerN (abs x)

----3

----4
reversalPrimes :: [Integer]
reversalPrimes = [p | p<- (take 10000 primes), p < 10001, prime (reversal p)]

----5
primeList :: Int->[Integer]
primeList n = sum (drop n (take (n+101) primes)): primeList (n+1)

checkIfPrime :: [Integer]
checkIfPrime = [p | p<- primeList 0, prime p]

firstPrime = head checkIfPrime

----6
--creating a infinite list which calculates the product ->[(p1×..×pn)+1] for every n 
mulPrimeList :: Int->[Integer]
mulPrimeList n = (product(take n primes)) + 1 : mulPrimeList (n+1)

--creating another infinite list which takes from mulPrimeList, all numbers that are not primes 
checkMulPrime = [p | p<- mulPrimeList 0, not( prime p)]

--taking the first element of checkMulPrime to find the answer
firstMulPrime = head checkMulPrime

----7
--digits: splits an integer into its digits
digits :: Integer -> [Int]
digits n = map (\x -> read [x] :: Int) (show n)

--doubleFunc takes a list and doubles every second digit from the list
doubleFunc ::[Int]->[Int]
doubleFunc [] =[]
doubleFunc (x:[]) =[x]
doubleFunc (x:y:xs) = x:(y*2):doubleFunc xs

--checkDouble checks if a number is higher than 9
checkDouble ::[Int]->[Int]
checkDouble [] = []
checkDouble (x:xs)
    | x > 9  = (x - 9) : checkDouble xs
    | otherwise = x : checkDouble xs 


insertAccount n = checkDouble (doubleFunc (digits (reversal n)))

luhn :: Integer->Bool
luhn n = (sum (insertAccount n)) `mod` 10 == 0

accountLength :: Integer->Int
accountLength n = length( digits n)

americanExpressDigitTest :: [Int]->Bool 
americanExpressDigitTest (x:y:xs) = (x ==3) && (y==4 || y==7)

isAmericanExpress :: Integer -> Bool
isAmericanExpress n = luhn n && (accountLength n)==15 && (americanExpressDigitTest ( digits n))

masterCardDigitTest :: [Int]->Bool 
masterCardDigitTest (x:y:xs) = (x ==5) && (y>0 && y<6)

isMasterCard :: Integer -> Bool
isMasterCard n = luhn n && (accountLength n)==16 && (masterCardDigitTest ( digits n))

VisaCardDigitTest :: [Int]->Bool 
VisaCardDigitTest (x:xs) = (x ==4)

isVisa :: Integer -> Bool
isVisa n = luhn n && (accountLength n)==16 && (VisaCardDigitTest ( digits n) )

----
--accuses :: Boy -> Boy -> Bool
--accuses Peter = Matthew||Jack
--accuses Matthew = not(Carl&&Matthew)

--accusers :: Boy -> [Boy]
--accusers :: Boy -> [Boy]



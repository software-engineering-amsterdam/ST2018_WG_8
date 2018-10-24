module ExamOct2018 where
import Data.List
import Test.QuickCheck
import Data.Char
import Data.Tuple
import Data.Map
import SetOrd
import System.Process
import System.Random
import Control.Monad
import Data.Bits


type Rel a = [(a,a)]

isAntiSymm :: Eq a => Rel a -> Bool
isAntiSymm r = not (elem False ([checkAntisym x r | x <- r]))

checkAntisym x xs = if (snd(x),fst(x)) `elem` xs then fst x == snd x else True

freqList :: String -> [(Char,Int)]
freqList s = toList $ fromListWith (+) [(c, 1) | c <- s]

testFreqlist :: String -> Bool
testFreqlist = (\s -> sumFreqs(freqList s) == length s)

sumFreqs :: [(Char, Int)] -> Int
sumFreqs [] = 0
sumFreqs (x:xs) = snd(x) + sumFreqs(xs)

testHuffProp :: String -> Bool
testHuffProp [] = True
testHuffProp s = prop_huffman (string2tree s)

createTree :: [(Char,Int)] -> HTree
createTree [] = error "empty input list"
createTree [(c,i)] = Leaf c i
createTree ((c,i):t) = merge (Leaf c i) (createTree t)

merge :: HTree -> HTree -> HTree
merge t1 t2 = Fork t1 t2 (weight t1 + weight t2)

prop_huffman :: HTree -> Bool
prop_huffman (Leaf _ _) = True
prop_huffman (Fork t1 t2 w) = prop_huffman t1 && prop_huffman t2
    && weight t1 + weight t2 == w

weight :: HTree -> Int
weight (Leaf _ w) = w
weight (Fork _ _ w) = w

string2tree :: String -> HTree
string2tree = createTree . freqList

data HTree =
    Leaf Char Int
    | Fork HTree HTree Int
    deriving (Show)


probSuccess1 :: Int -> Rational -> Rational
probSuccess1 k p = let q = 1 - p in
    sum [ p * q^m | m <- [1..k-1] ]

probSuccess2 :: Int -> Rational -> Rational
probSuccess2 k p = 1 - (1 - p)^k


probSuccess3 :: Int -> Rational -> Rational
probSuccess3 k p = let q = 1 - p in
    ((q^k - 1) / (q - 1)) * p

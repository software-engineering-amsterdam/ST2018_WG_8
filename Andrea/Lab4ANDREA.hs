module Lab4 where

import Data.List
import System.Random
import Test.QuickCheck
import SetOrd

{- Exercise 1: Questions. -}

{- 
    Exercise 2: Implement a random data generator for the datatype Set Int,
    where Set is as defined in SetOrd.hs. First do this from scratch,
    next give a version that uses QuickCheck to random test this datatype.
-}

-- instance Arbitrary Set Int where
--     arbitrary = 
--         sized $ \n -> do 
--             k <- choose (0, n)
--             sequence [ arbitrary | _ <- [1..k]]

{- 
    Exercise 3: Implement operations for set intersection, set union
    and set difference, for the datatype Set defined in SetOrd.hs.
    Next, use automated testing to check that your implementation is correct.
    First use your own generator, next use QuickCheck.
-}


{- Exercise 4: . -}
{- 
    Exercise 5: Give a function symClos that gives the symmetric closure
    of a relation, where the relation is represented as an ordered list of pairs.
    E.g., symClos [(1,2),(2,3),(3,4)] should give [(1,2),(2,1),(2,3),(3,2),(3,4),(4,3)].
    Deliverable: Haskell program, indication of time spent.
-}

type Rel a = [(a,a)]

symClos :: Ord a => Rel a -> Rel a
symClos [] = []
symClos ((a,b) : xs) = 
    if (b, a) `elem` xs
        then (a, b) : symClos xs
        else (a, b) : (b, a) : symClos xs                    


{- Exercise 6: . -}
{- Exercise 7: . -}
{- Exercise 8: . -}
{- Exercise 9: Bonus. -}
{- Exercise 10: Superbonus. -}


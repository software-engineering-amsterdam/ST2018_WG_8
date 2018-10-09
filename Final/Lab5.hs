module Lab5 where

import Data.List
import System.Random
import Lecture5

{-
    Exercise 1 is in Lecture5Exercise1.hs
    Deliverables: modified Sudoku solver, solution to the above puzzle, indication of time spent.
-}

{-
    Exercise 2 is in Lecture5Exercise2.hs
    Deliverables: Refactored code, test report, indication of time spent.
-}

{-
    Exercise 3 is in Lecture5Exercise34.hs
    Deliverables: testing code, test report, indication of time spent.
-}

{-
    Exercise 4 is in Lecture5Exercise2.hs
    Deliverables: generator, short report on findings, indication of time spent.
-}

{-
    Exercise 5:

    Extend the code of the lectures to create a program that generates NRC
    Sudoku problems, that is, Sudoku problems satisfying the extra constraint
    explained in the NRC exercise above.

    Deliverables: NRC Sudoku generator, indication of time spent.
-}

-- I created a prime functions for all functions that use the prune function
-- and Anna added the constraint for NRC sudokus to the prune function.
-- The generator will call these functions eventually and instead of generate
-- a regular sudoku, it will generate one that adheres to the NRC constraints.
prune' :: (Row,Column,Value) -> [Constraint] -> [Constraint]
prune' _ [] = []
prune' (r,c,v) ((x,y,zs):rest)
    | r == x = (x,y,zs\\[v]) : prune' (r,c,v) rest
    | c == y = (x,y,zs\\[v]) : prune' (r,c,v) rest
    | sameblock (r,c) (x,y) =
        (x,y,zs\\[v]) : prune' (r,c,v) rest
    | newsameblock (r,c) (x,y) =
        (x,y,zs\\[v]) : prune' (r,c,v) rest--added/ checks newsameblock
    | otherwise = (x,y,zs) : prune' (r,c,v) rest

search' :: (node -> [node])
       -> (node -> Bool) -> [node] -> [node]
search' children goal [] = []
search' children goal (x:xs)
  | goal x    = x : search' children goal xs
  | otherwise = search' children goal ((children x) ++ xs)

solveNs' :: [Node] -> [Node]
solveNs' = search' succNode' solved

succNode' :: Node -> [Node]
succNode' (s,[]) = []
succNode' (s,p:ps) = extendNode' (s,ps) p

extendNode' :: Node -> Constraint -> [Node]
extendNode' (s,constraints) (r,c,vs) =
   [(extend s ((r,c),v),
     sortBy length3rd $
         prune' (r,c,v) constraints) | v <- vs ]


rsuccNode' :: Node -> IO [Node]
rsuccNode' (s,cs) = do
    xs <- getRandomCnstr cs
    if null xs then return [] else return (extendNode' (s,cs\\xs) (head xs))

rsolveNs' :: [Node] -> IO [Node]
rsolveNs' ns = rsearch rsuccNode' solved (return ns)

-- This generates a solved sudoku NRC style
genRandomNRCSudoku :: IO Node
genRandomNRCSudoku = do
    [r] <- rsolveNs' [emptyN]
    return r

-- This function minimizes the solved sudoku in order to make it a sudoku puzzle.
genNRCProblem :: IO Node
genNRCProblem = do
    x <- genRandomNRCSudoku
    x' <- genProblem x
    return x'

{-
    Exercise 6 (Bonus): Can you find a way of classifying the difficulty of a Sudoku problem? 
    https://arxiv.org/pdf/1403.7373.pdf
-}

-- Sudoku difficulties are based on what techniques you need to use, not per say the amount of filled positions.
-- See besides Pelanek: http://www.sudokuoftheday.com/techniques/ 
-- and https://www.technologyreview.com/s/428729/mathematics-of-sudoku-leads-to-richter-scale-of-puzzle-hardness/

-- To compare: the hardest sudoku currently known to man has 21 positions filled 
-- (according to the "Richter" scale).
-- while the sudoku with the least amount of clues known has 17.
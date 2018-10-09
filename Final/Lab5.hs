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
    Exercise 4 is in Lecture5Exercise34.hs
    Deliverables: generator, short report on findings, indication of time spent.
-}

{-
    Exercise 5 is in Lecture5Exercise5.hs
    Deliverables: NRC Sudoku generator, indication of time spent.
-}

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

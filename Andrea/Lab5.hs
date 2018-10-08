module Lab5
    where

import Data.List
import Lecture5
-- import Lecture52
import System.Random

{-
    Exercise 1: Your task is to formalize this extra constraint,
    and to use your formalization in a program that can solve this
    Sudoku. See also the webpage of Andries Brouwer.
    Deliverables: modified Sudoku solver, solution to the above puzzle, indication of time spent.
-}

{-
    1 hour (incl. sudoku solution on paper)
    Code is also in Lecture5.hs

    nrcBlocks :: [[Int]]
    nrcBlocks = [[2..4],[6..8]]

    nrcBl :: Int -> [Int]
    nrcBl x = concat $ filter (elem x) nrcBlocks 

    freeInNrcgrid :: Sudoku -> (Row,Column) -> [Value]
    freeInNrcgrid s (r,c) = freeInSeq (nrcGrid s (r,c))

    nrcInjective :: Sudoku -> (Row, Column) -> Bool
    nrcInjective s (r,c) = injective vs where 
        vs = filter (/= 0) (nrcGrid s (r,c))

    prune::
    | sameNrcblock (r,c) (x,y) = 
        (x,y,zs\\[v]) : prune (r,c,v) rest
-}

-- Problem 1 is the given sudoku, has many solutions, 1 with NRC constraint.
problem1 :: Grid
problem1 = [[0,0,0,3,0,0,0,0,0],
            [0,0,0,7,0,0,3,0,0],
            [2,0,0,0,0,0,0,0,8],
            [0,0,6,0,0,5,0,0,0],
            [0,9,1,6,0,0,0,0,0],
            [3,0,0,0,7,1,2,0,0],
            [0,0,0,0,0,0,0,3,1],
            [0,8,0,0,4,0,0,0,0],
            [0,0,2,0,0,0,0,0,0]]

solution1 :: Grid
solution1 = [[4,7,8,3,9,2,6,1,5],
             [6,1,9,7,5,8,3,2,4],
             [2,3,5,4,1,6,9,7,8],
             [7,2,6,8,3,5,1,4,9],
             [8,9,1,6,2,4,7,5,3],
             [3,5,4,9,7,1,2,8,6],
             [5,6,7,2,8,9,4,3,1],
             [9,8,3,1,4,7,5,6,2],
             [1,4,2,5,6,3,8,9,7]]

-- Problem 2 has 2 solutions.
problem2 :: Grid
problem2 = [[2,9,5,7,4,3,8,6,1],
             [4,3,1,8,6,5,9,0,0],
             [8,7,6,1,9,2,5,4,3],
             [3,8,7,4,5,9,2,1,6],
             [6,1,2,3,8,7,4,9,5],
             [5,4,9,2,1,6,7,3,8],
             [7,6,3,5,2,4,1,8,9],
             [9,2,8,6,7,1,3,5,4],
             [1,5,4,9,3,8,6,0,0]]

problem3 :: Grid
problem3 = [[0,0,2,0,1,0,0,0,0],
            [1,0,0,5,0,0,0,0,3],
            [0,8,0,0,3,0,6,0,0],
            [0,7,9,0,0,0,0,0,0],
            [0,0,0,9,0,0,3,0,1],
            [8,0,0,0,0,0,2,0,0],
            [0,0,0,0,0,9,0,0,0],
            [0,0,1,0,0,0,0,4,9],
            [0,0,5,0,0,4,1,8,0]]
{-
    Exercise 2: Refactor the code along the lines of this proposal,
    and next compare the two versions for extendability and efficiency.
    Which of the two versions is easier to modify for NRC sudokus,
    and why? Which of the two versions is more efficient?
    Devise your own testing method for this, and write a short test report.
    Deliverables: Refactored code, test report, indication of time spent.
-}

{-
    Exercise 3:

    A Sudoku problem P is minimal if it admits a unique solution, and every
    problem P' you can get from P by erasing one of the hints admits more than
    one solution. How can you test whether the problems generated by the code
    given in the lecture notes are minimal?

    Deliverables: testing code, test report, indication of time spent.
-}

-- Each problem P is minimal if solved only returns 1 Node in the returned [Node].

-- Helper function that prints the amount of solutions for a certain grid (maxed out at 10.)
printSolutions :: Grid -> IO Int
printSolutions gr = do
    if not (longerThan 10 (solveNs (initNode gr)))
        then return $ length(solveNs (initNode gr))
        else return 1000000

-- Bool to test if there is 1 solution to a sudoku.
testMinimalism :: Grid -> Bool
testMinimalism gr = not (longerThan 1 (solveNs (initNode gr))) &&
                    length (solveNs (initNode gr)) == 1

-- Functions to check if a list is longer than a certain amount of elements.
-- Using instead of length :: https://stackoverflow.com/questions/7371730/how-to-tell-if-a-list-is-infinite
isNonEmpty :: [a] -> Bool
isNonEmpty [] = False
isNonEmpty (_:_) = True

longerThan :: Int -> [a] -> Bool
longerThan n xs = isNonEmpty $ drop n xs

check :: Sudoku -> [(Row,Column)] -> Bool
check sud [] = True
check sud (x:xs) = not (testMinimalism(sud2grid(eraseS sud x))) && check sud xs

checkMinimalismLessHints :: Grid -> Bool
checkMinimalismLessHints gr = check sud (filledPositions sud)
    where sud = grid2sud gr

testCheck :: IO Bool
testCheck = do 
    [sud] <- rsolveNs [emptyN]
    minSud  <- genProblem sud
    return $ checkMinimalismLessHints (sud2grid (fst minSud))
    -- I used Anna's mind.

{-
    Exercise 4:

    Write a program that generates Sudoku problems with three empty blocks.
    Is it also possible to generate Sudoku problems with four empty blocks?
    Five? How can you check this?

    Deliverables: generator, short report on findings, indication of time spent.
-}

-- genSudoku :: Int -> Grid
-- genSudoku x = 
    -- genRandomSudoku (81 - x)

-- (length (filledPositions generatedProblem) == (81 - x))

getSudoku :: IO Node -> IO Sudoku
getSudoku x = do
    node <- x
    return $ fst node


{-
    Exercise 5:

    Extend the code of the lectures to create a program that generates NRC
    Sudoku problems, that is, Sudoku problems satisfying the extra constraint
    explained in the NRC exercise above.

    Deliverables: NRC Sudoku generator, indication of time spent.
-}


{-
    Exercise 6 (Bonus):

    Can you find a way of classifying the difficulty of a Sudoku problem? Can
    you modify the Sudoku problem generator so that it can generate problems
    that are minimal, but easy to solve by hand? Problems that are minimal but
    hard to solve by hand? How can you test whether the problems your program
    generates satisfy these properties? Consult (Pelánek 2014).
-}

{-
    Exercise 7 (Bonus):

    Minimal problems for NRC Sudokus need fewer hints than standard Sudoku problems.
    Investigate the difference. What is the average number of hints in a minimal
    standard Sudoku problem? What is the average number of hints in a minimal NRC
    Sudoku problem?
-}

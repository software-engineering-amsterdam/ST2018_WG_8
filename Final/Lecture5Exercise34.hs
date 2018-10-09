module Lecture5Exercise34

where

import Lecture5
import Test.QuickCheck

{-
    Exercise 3:

    A Sudoku problem P is minimal if it admits a unique solution, and every
    problem P' you can get from P by erasing one of the hints admits more than
    one solution. How can you test whether the problems generated by the code
    given in the lecture notes are minimal?

    Deliverables: testing code, test report, indication of time spent.
-}

-- Modified uniqueSol. 
-- We only need to know if # solutions > 1, not to find all of them.
uniqueSol' :: Node -> Bool
uniqueSol' node = singleton (take 2 (solveNs [node])) where
    singleton [] = False
    singleton [x] = True
    singleton [x, y] = False

-- Checking if erasing one of the hints admits more than one solution
check :: Node -> [(Row,Column)] -> Bool
check n [] = True
check n (x:xs) = not (uniqueSol' (eraseN n x)) && check n xs

checkMinimalismLessHints :: Node -> Bool
checkMinimalismLessHints n = check n (filledPositions (fst n))

-- Generator from Lecture5. 
-- We check if the generated sudoku has only one solution and
-- if erasing one of the hints admits more than one solution
checkGenerator :: IO Bool
checkGenerator = do [r] <- rsolveNs [emptyN]
                    s  <- genProblem r
                    return (uniqueSol' s && checkMinimalismLessHints s)

-- Testing the generator. It's really slow,
-- but will eventually print if the generated problems are minimal.
testingForMinimal :: Int -> IO Bool
testingForMinimal 0 = do
    return True
testingForMinimal n = do
    rest <- (testingForMinimal (n - 1))
    x <- checkGenerator
    return (x && rest)

{-
    Exercise 4:
    Write a program that generates Sudoku problems with three empty blocks.
    Is it also possible to generate Sudoku problems with four empty blocks?
    Five? How can you check this?

    We interpreted blocks as whole grids of 3x3 squares.

    It depends on what you call a 'problem'. Our generated sudokus here can still be 
    minimalized and have unique solutions. If you remove more than 5 blocks however,
    it will no longer be a unique solution.
    Deliverables: generator, short report on findings, indication of time spent.
-}

-- Create a list with all the positions
allPositions = [(r,c) | r <- [1..9], c <- [1..9]]

deleteBlock :: Node -> (Row,Column) -> Node
deleteBlock n (r,c) = foldr (\rc n' -> eraseN n' rc) n (sameBlock (r,c))

-- Create a list with all the positions which are included in a subgrid (given one position of this subgrid)
sameBlock :: (Row, Column) -> [(Row, Column)]
sameBlock (r,c) = [(x,y)| x <- bl r, y<- bl c]

-- Check if the subgrids of two positions are in the same row or column
-- We don't want the three empty subgrids to be all in the same row or column
checkRC :: (Row, Column) -> (Row, Column) ->Bool
checkRC a b = (bl (fst a) /= bl (fst b) && bl (snd a) /= bl (snd b))

-- We choose 3 random positions. We make sure that they are in different subgrids and that their subgrids 
-- are not all in the same row or column.
-- We delete the 3 subgrids in which our positions are included
deleteBlocks :: Node -> IO Node
deleteBlocks n = do list1 <- randomize allPositions
                    let delBlock1 = head list1
                    let list2 =  [a | a <- list1, not (a `elem` (sameBlock delBlock1))]
                    let delBlock2 = head list2
                    let list3 =  [a | a <- list2, not (a `elem` (sameBlock delBlock2)), (checkRC a delBlock2) || (checkRC a delBlock1)]
                    let delBlock3 = head list3
                    let delBlockList = [delBlock1,delBlock2,delBlock3]
                    let newNode = foldr (\b n' -> deleteBlock n' b) n delBlockList
                    return newNode

randomSudoku :: IO ()
randomSudoku = do [r] <- rsolveNs [emptyN]
                  showNode r
                  sudokuEmptyblocks <- deleteBlocks r
                  s <- genProblem sudokuEmptyblocks --making this a sudoku problem
                  showNode s

main34 :: IO ()
main34 = do
    putStrLn("Exercise 3:")
    putStrLn("Testing with 10 tests because it is really slow:")
    result <- (testingForMinimal 10)
    print(result)

    putStrLn("Exercise 4:")
    putStrLn("Our generator doesn't always create sudokus with a unique solution, but they are still sudoku problems. \nBelow is a generated sudoku with 3 empty blocks:")
    r <- randomSudoku
    print(r)

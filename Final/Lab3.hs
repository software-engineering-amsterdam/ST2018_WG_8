module Lab3 where

import Data.List
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Lecture3
import System.Process
import Control.Monad

randomNumber :: IO Int
randomNumber = head <$> (replicateM 1 $ randomRIO (1,28))

--  Ex 1

tautology :: Form -> Bool
tautology f = all (\ v -> evl v f) (allVals f)

--always false, so it cannot be satisfiable
contradiction :: Form -> Bool
contradiction f = not(satisfiable f)

entails :: Form-> Form -> Bool
entails f g = tautology (Impl f g)

equivalence :: Form -> Form -> Bool
equivalence f g = tautology (Equiv f g)


-- Testing the definitions, first contradiction. Any satisfiable function
-- should not be a contradiction, vice-versa: a contradiction can never be
-- satisfiable.
testContradiction :: IO Bool
testContradiction = do
    x <- generateForm
    let editedX = (contradiction (nnf (arrowfree x)))
    if (contradiction x) then return ((not (satisfiable x)) && editedX) else return (not editedX)

testTautology :: IO Bool
testTautology = do
    x <- generateForm
    let editedX = (tautology (nnf (arrowfree x)))
    if (tautology x) then return ((satisfiable x) && editedX) else return (not editedX)

testEquivalence :: IO Bool
testEquivalence = do
    x <- generateForm
    let editedX = (nnf (arrowfree x))
    return (equivalence x editedX)

-- Entails always accept two instances of the same form (automatically tautologies) and
-- all contradictions.The same form is valid due to the fact if a is true then a is also true.
-- A set of tautologies should be accepted because if both are always true, entails holds.
-- Finally, a contradiciton provides false in the first instance so the second one is not
-- evaluated.
testEntails :: IO Bool
testEntails = do
    f1 <- generateForm
    f2 <- generateForm
    if (contradiction f1)
        then return (entails f1 f2)
        else if (contradiction f2)
            then return (entails f2 f1)
            else if (tautology f1 && tautology f2) || f1 == f2
                then return ((entails f1 f2) && (entails f2 f1))
                else if (equivalence f1 f2)
                    then return ((entails f1 f2))
                    else return True

testFunc :: Integer -> (IO Bool) -> IO Bool
testFunc 0 f = do
    return True
testFunc n f = do
    t <- f
    rest <- (testFunc (n - 1) f)
    return (t && rest)


-- Ex 4
-- This function uses the itemPicker to generate proper forms (longer that a single
-- prop.) It checks if its a properly long form before returning, otherwise a new
-- one is generated.
generateLogic :: IO String
generateLogic = do
    randomNum <- randomNumber
    let form = itemPicker randomNum
    f <- form
    if ((length f) > 10) && ((length f) < 50) then return f else generateLogic

-- This function parses a generated random logic string into a [Form].
-- It can thus be used also in checking the parser for a much larger variaty of
-- strings.
generateForm :: IO Form
generateForm  = do
    s <- generateLogic
    let parsed = parse s
    return (head parsed)

-- This function is the heart of the generator, it uses a random number to choose
-- a form from among its options. Disjunctions and conjunctions are limited to
-- 4 parts, negations, implications and equalities are generated with 1 and 2
-- parts respectively. The parts in each dsj, cnj etc, are generated recursively.
-- This means conjunctions can contain themselves or any other element etc.
-- In order to make this work the amount of recursions called must never outweigh
-- the amount of props generated (it will probably run forever if it gains momentum).
-- Therefore the first 18 numbers generated are props, the other 10 are the
-- rest of the possible forms.
itemPicker :: Int -> IO [Char]
itemPicker n
    | n <= 18 = do
        return (show n)
    | n <= 20 = do
        randomNum <- randomNumber
        f1 <- itemPicker randomNum
        return ("-" ++ (f1) ++ "")
    | n <= 21 = do
        randomNum <- randomNumber
        f1 <- itemPicker randomNum
        randomNum <- randomNumber
        f2 <- itemPicker randomNum
        return ("+(" ++ (f1) ++ " " ++ (f2) ++ ")")
    | n <= 22 = do
        randomNum <- randomNumber
        f1 <- itemPicker randomNum
        randomNum <- randomNumber
        f2 <- itemPicker randomNum
        randomNum <- randomNumber
        f3 <- itemPicker randomNum
        return ("+(" ++ (f1) ++ " " ++ (f2) ++ " " ++ (f3) ++ ")")
    | n <= 23 = do
        randomNum <- randomNumber
        f1 <- itemPicker randomNum
        randomNum <- randomNumber
        f2 <- itemPicker randomNum
        randomNum <- randomNumber
        f3 <- itemPicker randomNum
        randomNum <- randomNumber
        f4 <- itemPicker randomNum
        return ("+(" ++ (f1) ++ " " ++ (f2) ++ " " ++ (f3) ++ " " ++ (f4) ++ ")")
    | n <= 24 = do
        randomNum <- randomNumber
        f1 <- itemPicker randomNum
        randomNum <- randomNumber
        f2 <- itemPicker randomNum
        return ("*(" ++ (f1) ++ " " ++ (f2) ++ ")")
    | n <= 25 = do
        randomNum <- randomNumber
        f1 <- itemPicker randomNum
        randomNum <- randomNumber
        f2 <- itemPicker randomNum
        randomNum <- randomNumber
        f3 <- itemPicker randomNum
        return ("*(" ++ (f1) ++ " " ++ (f2) ++ " " ++ (f3) ++ ")")
    | n <= 26 = do
        randomNum <- randomNumber
        f1 <- itemPicker randomNum
        randomNum <- randomNumber
        f2 <- itemPicker randomNum
        randomNum <- randomNumber
        f3 <- itemPicker randomNum
        randomNum <- randomNumber
        f4 <- itemPicker randomNum
        return ("*(" ++ (f1) ++ " " ++ (f2) ++ " " ++ (f3) ++ " " ++ (f4) ++ ")")
    | n <= 27 = do
        randomNum <- randomNumber
        f1 <- itemPicker randomNum
        randomNum <- randomNumber
        f2 <- itemPicker randomNum
        return ("(" ++ (f1) ++ " ==> " ++ (f2) ++ ")")
    | n <= 28 = do
        randomNum <- randomNumber
        f1 <- itemPicker randomNum
        randomNum <- randomNumber
        f2 <- itemPicker randomNum
        return ("(" ++ (f1) ++ " <=> " ++ (f2) ++ ")")
    | otherwise = do
        return "1"

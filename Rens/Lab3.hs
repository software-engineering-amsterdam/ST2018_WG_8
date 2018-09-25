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

-- Exercise 1 ( 1 hour)
contradiction, tautology :: Form -> Bool
tautology f = all (\ v -> evl v f) (allVals f)
contradiction f = not (satisfiable f)

entails :: Form -> Form -> Bool
entails f1 f2 = all (\ v -> (evl v f1) --> (evl v f2)) (genVals (nub ((propNames f1) ++ (propNames f2))))

equivalence :: Form -> Form -> Bool
equivalence f1 f2 = (entails f1 f2) && (entails f2 f1)

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

-- Exercise 2
-- Lets test if the parser accepts strings that should be accepted by
-- entering a sequence of strings and checking if all correct items are properly parsed.
testParser :: String -> Bool
testParser x = length (filter (/= ' ') (show (parse x))) == (elementsIn x)
    where
        elementsIn s = 2 + length (filter (/= ' ') s)

testBasicsParser = ["(1 ==> 2)","(1 <=> 2)","+ (1 1)","-1","* (1 1)","1"]
testCompositionParser = ["*((2 ==> 3) -3)", "*(+(2 3) -3)", "(*(1 1) <=> -+(1 +(2 3)))"]

-- Exercise 3
-- Data definitions of the literal, clause and conjunction.
-- data L = Neg Form | Form
-- data C = L | Dsj [C, L]
-- data D = C | Cnj [D, C]

-- A precondition for the running of cnf is that it must be arrowfree as produced
-- by the arrowfree function.
cnf :: Form -> Form
cnf (Prop x) = Prop x
cnf (Neg f) = Neg (cnf f)
cnf (Dsj [Cnj [x,y],z]) = Cnj [cnf (Dsj [z,x]), cnf (Dsj [z,y])]
cnf (Cnj [z, Cnj [x, y]]) = Cnj [cnf (Dsj [z,x]), cnf (Dsj [z,y])]
cnf (Cnj fs) = Dsj (map cnf fs)
cnf (Dsj fs) = Dsj (map cnf fs)

-- isCnf is used in converting to cnf as a form must be checked over and over
-- untill it is cnf, if not it has to be rerun (as this runs inward it can take
-- a long time for long forms).
isCnf :: Form -> Bool
isCnf (Prop x) = True
isCnf (Neg (Prop x)) = True
isCnf (Neg _) = False
isCnf (Dsj xs) = ((not . any) innerCnj xs) && (all (== True) (map innerCnj xs))
isCnf (Cnj xs) = all (== True) (map innerCnj xs)

-- Simple function that checks if an element is a conjucntion.
innerCnj :: Form -> Bool
innerCnj (Cnj xs) = True
innerCnj _ = False

{-
    This only works for forms with 2 part conjunctions or disjunctions unfortunately.
    Pieter Donkers explained a way to turn it into subpairs but we were not able
    to properly convert it within the time we had. This can be mitigated by running
    the generator with a max of 2 cnj/dsj's.
-}

toCnf :: Form -> Form
toCnf f = while (\x -> not (isCnf x)) (cnf f)

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

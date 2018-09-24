module Lab3 where

import Data.List
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Lecture3
import System.Process
import Control.Monad

testTaut = Dsj [Prop 1, (Neg (Prop 1))]
testContra = Cnj [Prop 1, (Neg (Prop 1))]
testEntails = Prop 1
testEquivalence = Prop 2

randomNumber :: IO Int
randomNumber = head <$> (replicateM 1 $ randomRIO (1,25))

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
testingContra :: Form -> Bool
testingContra = \f -> if (contradiction f) then not (satisfiable f) else True

-- A tautology should always be satisfiable and never a contradiction.
testingTaut :: Form -> Bool
testingTaut = \f -> (tautology f) --> ((satisfiable f) && (not (contradiction f)))

-- Entails always accept two instances of the same form (automatically tautologies) and
-- all contradictions.The same form is valid due to the fact if a is true then a is also true.
-- A set of tautologies should be accepted because if both are always true, entails holds.
-- Finally, a contradiciton provides false in the first instance so the second one is not
-- evaluated.
testingEntails :: Form -> Form -> Bool
testingEntails f1 f2 = if (contradiction f1)
    then (entails f1 f2)
    else if (contradiction f2)
        then (entails f2 f1)
        else if (tautology f1 && tautology f2) || f1 == f2
            then (entails f1 f2) && (entails f2 f1)
            else if f1 == f2
                then (entails f1 f2)
                else True

-- Exercise 2
-- Lets test if the parser accepts strings that should be accepted by
-- entering a sequence of strings and checking if all correct items are properly parsed.
testParser :: [String] -> Bool
testParser [] = True
testParser (x:xs) = length (filter (/= ' ') (show (parse x))) == (elementsIn x) && testParser xs
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
cnf (Dsj [Cnj fs]) = Neg (Cnj [Dsj (map cnf fs)])
cnf (Cnj fs) =  Cnj (map cnf fs)
cnf (Dsj fs) = Dsj (map cnf fs)

-- randomFormGenerator :: Integer -> [Form]
-- randomFormGenerator 0 = []
-- randomFormGenerator n = generateForm : randomFormGenerator (n - 1)

generateForm = do
    randomNum <- randomNumber
    print randomNum
    let form = itemPicker randomNum
    f <- form
    print f
    return form

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

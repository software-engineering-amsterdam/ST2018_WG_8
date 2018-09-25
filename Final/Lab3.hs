module Lab3 where

import Control.Monad
import Data.List
import Lecture3
import System.Process
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Monadic

randomNumber :: IO Int
randomNumber = head <$> (replicateM 1 $ randomRIO (1,28))


{- 
    Exercise 1: Give definitions.
    Deliverables: implementation, description of your method
    of checking the definitions, indication of time spent.
-}

-- Tautology: All statements satisfy.
tautology :: Form -> Bool
tautology f = all (\ v -> evl v f) (allVals f)

-- Contradiction: No statements satisfy.
contradiction :: Form -> Bool
contradiction f = not (satisfiable f)

-- Logical entailment: B logically entails A is true if and only if
-- all of the truth valuations that satisfy B also satisfy A.
entails :: Form-> Form -> Bool
entails f g = tautology (Impl f g)

-- Logical equivalence: A & B are equivalent.
equivalence :: Form -> Form -> Bool
equivalence f g = tautology (Equiv f g)

-- Testing below.
testContradiction :: IO Bool
testContradiction = do
    x <- generateForm
    let simplifiedX = (contradiction (nnf (arrowfree x)))
    if (contradiction x) 
        then return ((not (satisfiable x)) && simplifiedX) 
        else return (not simplifiedX)

testTautology :: IO Bool
testTautology = do
    x <- generateForm
    let simplifiedX = (tautology (nnf (arrowfree x)))
    if (tautology x) 
        then return ((satisfiable x) && simplifiedX) 
        else return (not simplifiedX)

testEquivalence :: IO Bool
testEquivalence = do
    x <- generateForm
    let simplifiedX = (nnf (arrowfree x))
    return (equivalence x simplifiedX)

{- 
    Entails always accept two instances of the same form (automatically tautologies) and
    all contradictions. The same form is valid due to the fact if a is true then a is also true.
    A set of tautologies should be accepted because if both are always true, entails holds.
    Finally, a contradiciton provides false in the first instance so the second one is not
    evaluated.
-}
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

-- Function to test our implementations.
testFunc :: Integer -> (IO Bool) -> IO Bool
testFunc 0 f = do
    return True
testFunc n f = do
    t <- f
    rest <- (testFunc (n - 1) f)
    return (t && rest)

{- 
    Exercise 2: Test the parsing function. You can use any test method you want.
    Deliverables: test report describing the test method used &&
    the outcome of the test, indication of time spent.
-}

-- Simple tests: Test if the parser accepts strings, that should be accepted by
-- entering a sequence of strings &&
-- checking if all correct items are properly parsed.
testParser :: String -> Bool
testParser x = length (filter (/= ' ') (show (parse x))) == (elementsIn x)
    where
        elementsIn s = 2 + length (filter (/= ' ') s)

testBasicsParser = ["(1 ==> 2)","(1 <=> 2)","+ (1 1)","-1","* (1 1)","1"]
testCompositionParser = ["*((2 ==> 3) -3)", "*(+(2 3) -3)", "(*(1 1) <=> -+(1 +(2 3)))"]

{- 
    Exercise 4: Write a formula generator for random testing of properties of propositional logic.
    Test exercise 3 with your solution.
    Deliverables: generator for formulas, sequence of test properties, test report, indication of time spent.
-}

-- This function uses the itemPicker to generate proper forms
-- (longer than a single prop, but also with max. length)
generateLogic :: IO String
generateLogic = do
    randomNum <- randomNumber
    let form = itemPicker randomNum
    f <- form
    if ((length f) > 10) && ((length f) < 50)
        then return f
        else generateLogic

-- This function parses a generated random logic string into a [Form].
-- It can thus be used also in checking the parser for a much larger variety of
-- strings.
generateForm :: IO Form
generateForm  = do
    s <- generateLogic
    let parsed = parse s
    return (head parsed)

{- 
    This beauty is Rens' work and the heart of the generator.
    It uses a random number to choose a form from among its options.
    Disjunctions and conjunctions are limited to 4 parts,
    negations, implications and equalities are generated with 1 and 2 parts
    respectively. 
    
    The parts in each dsj, cnj, etc. are generated recursively.
    This means conjunctions can contain themselves or any other element.
    In order to make this work the amount of recursions called must never outweigh
    the amount of props generated (it will probably run forever if it gains momentum).
    Therefore the first 18 numbers generated are props, the other 10 are the
    rest of the possible forms.
-} 
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


{- 
    Exercise 5 (bonus): Write a program for converting formulas to clause form.
    Deliverables: Conversion program, test generator, test properties,
    documentation of the automated testing process.
    Also, give an indication of time spent.
-}

{- 
    NOTES ::
    L = p | -p
    C = L | L v C
    D = C | C ^ D
    CNF: (p v -p) ^ (q v -q) ^ (r v -r)

    EXAMPLES :: 
    [5, -6] == p5 v -p6
    [[4], [5, -6]] = p4 ^ (p5 V -p6)
-}

type Clause  = [Int]
type Clauses = [Clause]

-- Main function to transform all elements of Form into [Int],
-- then create a list of it, resulting into [Clause].
cnf2cls :: Form -> Clauses
cnf2cls (Cnj ps) = [ cnj2cls p | p <- ps]


-- CNF can exist out of L, C or D (see NOTES). Transform each in their 
-- appropriate clause counterpart. 
cnj2cls :: Form -> Clause
cnj2cls (Prop p) = [p]
cnj2cls (Neg (Prop p)) = [-p]
cnj2cls (Dsj ps) = [ dsj2cls p | p <- ps]

dsj2cls :: Form -> Int
dsj2cls (Prop p) = p
dsj2cls (Neg (Prop p)) = -p

exercise5 = do
    putStrLn("Testing exercise 5.")
    
    print cnjP
    print (cnf2cls cnjP)


    print cnjQ
    print (cnf2cls cnjQ)



-- Basic testing for now, add automatic testing later.
-- Simple conjunctions & disjunctions to test. 

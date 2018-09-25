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

-- Simple conjunctions & disjunctions to test.
cnjP, cnjQ, dsjP, dsjQ :: Form
cnjP = Cnj [p, Neg p]
cnjQ = Cnj [q, Neg q]
dsjP = Dsj [p, Neg p]
dsjQ = Dsj [q, Neg q]

{-
    Exercise 1: Give definitions.
    Deliverables: implementation, description of your method
    of checking the definitions, indication of time spent.
-}

-- Check if the function is a Tautology by using allVals on the evl of two funcs.
tautology :: Form -> Bool
tautology f = all (\ v -> evl v f) (allVals f)

-- Use an inversion of the satisfiable as the contradicion function.
contradiction :: Form -> Bool
contradiction f = not (satisfiable f)

-- Logical entailment: B logically entails A is true if and only if
-- all of the truth valuations that satisfy B also satisfy A.
entails :: Form-> Form -> Bool
entails f g = tautology (Impl f g)

-- Equivalence checks if two forms imply eachother and uses tautology to check
-- if this goes for all inputs.
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

-- Tautology and equivalence are tested by running it on two equivalent functions
-- and making sure the output is the same.
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

-- This function accepts a testable function and runs it for n trials of generated
-- logic forms.
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

{-
    To test the parser I will feed it a large sample (around 1M) of logic forms
    generated by the generator in exercise 4. This implies that exercise 4 is
    actually our language specification for the parser. When a form in string form
    gets parsed, the spaces are removed. Therefore we can compare the parsed form
    to the original string (+ 2 for the square brackets). If the form is incorrectly
    parsed only part of the input will appear in the parsed form or nothing at all.
-}
testParser :: String -> Bool
testParser x = length (filter (/= ' ') (show (parse x))) == (elementsIn x)
    where
        elementsIn s = 2 + length (filter (/= ' ') s)

testNSamples :: Integer -> IO Bool
testNSamples 0 = do
    return True
testNSamples n = do
    f <- generateLogic
    let t = testParser f
    ts <- testNSamples (n - 1)
    return (t && ts)

{-



-}

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
isCnf (Dsj xs) = (not (any innerCnj xs)) && (all (== True) (map innerCnj xs))
isCnf (Cnj xs) = all (== True) (map innerCnj xs)

-- Simple function that checks if an element is a conjucntion.
innerCnj :: Form -> Bool
innerCnj (Cnj xs) = True
innerCnj _ = False

{-
    This only works for forms with 2 part conjunctions or disjunctions unfortunately.
    Pieter Donkers explained a way to turn it into subpairs but we were not able
    to properly convert it within the time we had. This could be mitigated by running
    the generator with a max of 2 cnj/dsj's. Requires input that is arrowfree and nnf.
    However, the nnf, arrowfree and cnf functions might produce longer than 3
    parts which means this function usually runs forever...
-}

toCnf :: Form -> Form
toCnf f = while (not . isCnf) cnf f

-- Finally lets test it. Run the function on a form and check if its equivalent
-- to the original.
testCnf :: Integer -> IO Bool
testCnf 0 = do
    return True
testCnf n = do
    -- Adhere to preconditions
    f <- generateForm
    let f' = nnf (arrowfree (f))
    let fcnf = toCnf f'
    fs <- testCnf (n - 1)
    return ((equivalence f fcnf) && fs)

{-
    Exercise 4: Write a formula generator for random testing of properties of propositional logic.
    Test exercise 3 with your solution.
    Deliverables: generator for formulas, sequence of test properties, test report, indication of time spent.
-}

-- This function uses the itemPicker to generate proper forms (longer that a single
-- prop.) It checks if its a properly long form before returning, otherwise a new
-- one is generated.
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
cnf2cls (Prop p) = [[p]]
cnf2cls (Neg (Prop p)) = [[-p]]
cnf2cls (Cnj p) = map cnj2cls p
cnf2cls (Dsj p) = map cnj2cls p

-- CNF can exist out of L, C or D (see NOTES). Transform each in their
-- appropriate clause counterpart.
cnj2cls :: Form -> Clause
cnj2cls (Prop p) = [p]
cnj2cls (Neg (Prop p)) = [-p]
cnj2cls (Dsj ps) = concatMap cnj2cls ps

-- Reverse functions, to create forms from the clauses.
cls2cnf :: Clauses -> Form
cls2cnf p = Cnj $ map cls2cnj p

cls2cnj :: Clause -> Form
cls2cnj p = Dsj $ map (\p -> if p < 0 then Neg (Prop (-p)) else Prop p) p

-- Simple conjunctions & disjunctions for testing.
cnjP, cnjQ, dsjP, dsjQ, cnjR :: Form
cnjP = Cnj [p, Neg p]
cnjQ = Cnj [q, Neg q]
dsjP = Dsj [p, Neg p]
dsjQ = Dsj [q, Neg q]
cnjR = Cnj [dsjP, dsjQ]
cnfS = [[4], [5, -6]]

-- Basic testing for now, add automatic testing later.
-- Simple conjunctions & disjunctions to test.

main :: IO String
main = do
    let n = 1000
    putStrLn ("Set sample size at: " ++ (show n))

    putStrLn "Testing Tautologies:"
    r <- (testFunc n testTautology)
    print r

    putStrLn "Testing Contradictions:"
    r <- (testFunc n testContradiction)
    print r

    putStrLn "Testing Entails:"
    r <- (testFunc n testEntails)
    print r

    putStrLn "Testing Equivalence:"
    r <- (testFunc n testEquivalence)
    print r

    putStrLn "Testing parser against generated logic forms:"
    r <- testNSamples n
    print r

    putStrLn "Would test the CNF here if it would be able to split CNJ/DSJ in pairs, now it runs indefinetly."
    -- r <- testCnf n
    putStrLn "** NO TEST **"

    putStrLn "Testing bonus exercise on convertion:"
    print cnjP
    print (cnf2cls cnjP)
    print cnjR
    print (cnf2cls cnjR)
    print cnfS
    print (cls2cnf cnfS)

    -- Did not have time to finish the testing on 5, just ran some simple tests.
    -- Would have liked to have written a small function that would compare the original input
    -- to transform & reverse transform, and check if it would be the original input again.

    return "Done"

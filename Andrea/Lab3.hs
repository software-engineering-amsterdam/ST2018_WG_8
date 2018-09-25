module Lab3 where
 
import Data.List
import Lecture3
import System.Random
import Test.QuickCheck
    
{- 
    Exercise 1: Give definitions.
    Deliverables: implementation, description of your method
    of checking the definitions, indication of time spent.
-}

-- tautology: All statements satisfy.
tautology :: Form -> Bool
tautology f = all (\v -> evl v f) (allVals f)

-- contradiction: No statements satisfy.
contradiction :: Form -> Bool
contradiction f = not (satisfiable f)

-- entails: B logically entails A is true if and only if
-- all of the truth valuations that satisfy B also satisfy A.
entails :: Form -> Form -> Bool
entails f g = all (\v -> evl v f --> evl v g) (allVals (Impl f g))

-- equiv: A & B are equivalent.
equiv :: Form -> Form -> Bool
equiv f g = entails f g && entails g f

-- Simple conjunctions & disjunctions to test. 
cnjP, cnjQ, dsjP, dsjQ :: Form
cnjP = Cnj [p, Neg p]
cnjQ = Cnj [q, Neg q]
dsjP = Dsj [p, Neg p]
dsjQ = Dsj [q, Neg q]

exercise1 = do
    print "Testing exercise 1. These should return TRUE:"
    -- Tautology & Contradiction:
    print (tautology dsjP)
    print (contradiction cnjP)

    -- Logical entailment & Logical equivalence:
    print (entails cnjP cnjQ)
    print (entails dsjP dsjQ)
    print (equiv dsjP dsjQ)
    print (equiv cnjP cnjP)

    print "Testing exercise 1. These should return FALSE:"
    -- Tautology & Contradiction:
    print (tautology cnjQ)
    print (contradiction dsjQ)

    -- Logical entailment & Logical equivalence:
    print (entails dsjP cnjQ)
    print (equiv cnjP dsjP)

{- 
    Exercise 2: Test the parsing function.
    Deliverables: test report describing the test method used
    and the outcome of the test, indication of time spent.
-}

-- The parseForm should produce (Forms, []), if the grammar is correct.

{-
    Exercise 3: Write a Haskell program for converting Boolean formulas into CNF.
    Deliverables: conversion program with documentation,
    indication of time spent.
-}

-- Function to check if the current Form is CNF.
-- isCNF :: Form -> Boolean
-- isCNF (Prop p) = True 
-- isCNF (Neg p) = Neg (isCNF p)

{- 
    Exercise 5 (bonus): Write a program for converting formulas to clause form.
    You may assume that your formulas are already in CNF.

    Deliverables: Conversion program, test generator, test properties,
    documentation of the automated testing process.
    Also, give an indication of time spent.
-}

{- 
    CNF: 
    L = p | -p
    C = L | L v C
    D = C | C ^ D
    CNF: (p v -p) ^ (q v -q) ^ (r v -r)

    Examples: 
    [5, -6] == p5 v -p6
    [[4], [5, -6]] = p4 ^ (p5 V -p6)
-}

-- Formula (CNF) to clause form. 
type Clause  = [Int]
type Clauses = [Clause]

-- Transform all elements of the form into [Int], concatenate into [Clause].
cnf2cls :: Form -> Clauses
cnf2cls (Cnj ps) = [cnj2cls p | p <- ps]

-- CNF can exist out of L, C and D (see above). Transform each in their 
-- appropriate clause counterpart. 
cnj2cls :: Form -> Clause
cnj2cls (Prop p) = [p]
cnj2cls (Neg p) = [-p]
cnj2cls (Dsj ps) = -- Do something.

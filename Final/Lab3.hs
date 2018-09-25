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

module ExamHelpers
    where
import Data.List
import Test.QuickCheck
import Data.Char
import Data.Tuple
import SetOrd
import System.Process
import System.Random
import Control.Monad

-- Helpers for sets.

type Rel a = [(a,a)]

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s =  nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

powerList  :: [a] -> [[a]]
powerList  [] = [[]]
powerList  (x:xs) = (powerList xs)
                     ++ (map (x:) (powerList xs))


-- Function to check two lists against each other.
-- On success, return true. Else, return false.
compareLists :: Ord a => [a] -> [a] -> Bool
compareLists [] [] = True
compareLists x [] = False
compareLists [] y = False
compareLists (x:xs) (y:ys) = x == y && compareLists xs ys

checkNotEq :: Eq a => [a] -> [a] -> Bool
checkNotEq [] [] = True
checkNotEq x [] = True
checkNotEq [] y = True
checkNotEq (x:xs) (y:ys) = not (x == y) && checkNotEq xs ys

-- Form to CNF (Pieters version)
strictCnf :: Form -> Form
strictCnf (Prop x)                 = Prop x
strictCnf (Neg x)                  = Neg (strictCnf x)

--if only one argument is given this can be simplified
strictCnf (Cnj (x:[])) = x
strictCnf (Dsj (x:[])) = x

-- Replace the (p v (q ^ r)) with ((p v q) ^ (p v r))
strictCnf (Dsj [Cnj [x,y],z]) = Cnj [strictCnf(Dsj [z,x]),strictCnf(Dsj [z,y])]
strictCnf (Dsj [z, Cnj[x,y]]) = Cnj [strictCnf(Dsj [z,x]),strictCnf(Dsj [z,y])]

-- Check every element
strictCnf (Dsj xs) = Dsj (map strictCnf xs)
strictCnf (Cnj xs) = Cnj (map strictCnf xs)

-- By enforcing that all the conjunction and disjunctions are in pair and not more we
-- can ensure that the form p v (q ^ r) is always true
-- If we dont this the functions crashed on inputs like Cnj [p,q,r]
toPairs :: Form -> Form
toPairs (Prop x) = Prop x
toPairs (Neg x) = Neg (toPairs x)
toPairs (Cnj (x:[])) = toPairs x
toPairs (Cnj (x:xs)) | (length xs) > 1 = Cnj [toPairs x,toPairs(Cnj xs)]
                     | otherwise = Cnj (map toPairs (x:xs))
toPairs (Dsj (x:[])) = toPairs x
toPairs (Dsj (x:xs)) | (length xs) > 1 = Dsj [toPairs x,toPairs(Dsj xs)]
                     | otherwise = Dsj (map toPairs (x:xs))

-- Check if the function is in cnf form.
isCnf :: Form -> Bool
isCnf (Prop x) = True
isCnf (Neg (Prop x)) = True
isCnf (Neg _) = False
isCnf (Dsj xs) = not (any isCnj xs) && (all (==True) (map isCnf xs))
isCnf (Cnj xs) = all (==True) (map isCnf xs)
isCnf (Impl x y) = False
isCnf (Equiv x y) = False

-- Because appearantly we cant do list comprehension and do ==Cnj we add this fucntion.
isCnj :: Form -> Bool
isCnj (Cnj xs) = True
isCnj _ = False

-- Helpers for forms
type Var = String
type Env = Var -> Integer

data Expr = I Integer
          | V Var
          | Add Expr Expr
          | Subtr Expr Expr
          | Mult Expr Expr
          deriving (Eq,Show)

eval :: Expr -> Env -> Integer
eval (I i) _ = i
eval (V name) env = env name
eval (Add e1 e2) env = (eval e1 env) + (eval e2 env)
eval (Subtr e1 e2) env = (eval e1 env) - (eval e2 env)
eval (Mult e1 e2) env = (eval e1 env) * (eval e2 env)

while :: (a -> Bool) -> (a -> a) -> a -> a
while = until . (not.)

type Name = Int

data Form = Prop Name
          | Neg  Form
          | Cnj [Form]
          | Dsj [Form]
          | Impl Form Form
          | Equiv Form Form
          deriving (Eq,Ord)

instance Show Form where
  show (Prop x)   = show x
  show (Neg f)    = '-' : show f
  show (Cnj fs)     = "*(" ++ showLst fs ++ ")"
  show (Dsj fs)     = "+(" ++ showLst fs ++ ")"
  show (Impl f1 f2)  = "(" ++ show f1 ++ "==>"
                           ++ show f2 ++ ")"
  show (Equiv f1 f2)  = "(" ++ show f1 ++ "<=>"
                           ++ show f2 ++ ")"

showLst,showRest :: [Form] -> String
showLst [] = ""
showLst (f:fs) = show f ++ showRest fs
showRest [] = ""
showRest (f:fs) = ' ': show f ++ showRest fs

propNames :: Form -> [Name]
propNames = sort.nub.pnames where
  pnames (Prop name) = [name]
  pnames (Neg f)  = pnames f
  pnames (Cnj fs) = concatMap pnames fs
  pnames (Dsj fs) = concatMap pnames fs
  pnames (Impl f1 f2)  = concatMap pnames [f1,f2]
  pnames (Equiv f1 f2) = concatMap pnames [f1,f2]

type Valuation = [(Name,Bool)]

-- | all possible valuations for lists of prop letters
genVals :: [Name] -> [Valuation]
genVals [] = [[]]
genVals (name:names) =
  map ((name,True) :) (genVals names)
  ++ map ((name,False):) (genVals names)

-- | generate all possible valuations for a formula
allVals :: Form -> [Valuation]
allVals = genVals . propNames

type ValFct = Name -> Bool

val2fct :: Valuation -> ValFct
val2fct = updates (\ _ -> undefined)

fct2val :: [Name] -> ValFct -> Valuation
fct2val domain f = map (\x -> (x,f x)) domain

evl :: Valuation -> Form -> Bool
evl [] (Prop c)    = error ("no info: " ++ show c)
evl ((i,b):xs) (Prop c)
     | c == i    = b
     | otherwise = evl xs (Prop c)
evl xs (Neg f)  = not (evl xs f)
evl xs (Cnj fs) = all (evl xs) fs
evl xs (Dsj fs) = any (evl xs) fs
evl xs (Impl f1 f2) = evl xs f1 --> evl xs f2
evl xs (Equiv f1 f2) = evl xs f1 == evl xs f2

data Token
      = TokenNeg
      | TokenCnj
      | TokenDsj
      | TokenImpl
      | TokenEquiv
      | TokenInt Int
      | TokenOP
      | TokenCP
 deriving (Show,Eq)

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs) | isSpace c = lexer cs
             | isDigit c = lexNum (c:cs)
lexer ('(':cs) = TokenOP : lexer cs
lexer (')':cs) = TokenCP : lexer cs
lexer ('*':cs) = TokenCnj : lexer cs
lexer ('+':cs) = TokenDsj : lexer cs
lexer ('-':cs) = TokenNeg : lexer cs
lexer ('=':'=':'>':cs) = TokenImpl : lexer cs
lexer ('<':'=':'>':cs) = TokenEquiv : lexer cs
lexer (x:_) = error ("unknown token: " ++ [x])

lexNum cs = TokenInt (read num) : lexer rest
     where (num,rest) = span isDigit cs

type Parser a b = [a] -> [(b,[a])]

succeed :: b -> Parser a b
succeed x xs = [(x,xs)]

parseForm :: Parser Token Form
parseForm (TokenInt x: tokens) = [(Prop x,tokens)]
parseForm (TokenNeg : tokens) =
  [ (Neg f, rest) | (f,rest) <- parseForm tokens ]
parseForm (TokenCnj : TokenOP : tokens) =
  [ (Cnj fs, rest) | (fs,rest) <- parseForms tokens ]
parseForm (TokenDsj : TokenOP : tokens) =
  [ (Dsj fs, rest) | (fs,rest) <- parseForms tokens ]
parseForm (TokenOP : tokens) =
  [ (Impl f1 f2, rest) | (f1,ys) <- parseForm tokens,
                         (f2,rest) <- parseImpl ys ]
   ++
  [ (Equiv f1 f2, rest) | (f1,ys) <- parseForm tokens,
                          (f2,rest) <- parseEquiv ys ]
parseForm tokens = []

parseForms :: Parser Token [Form]
parseForms (TokenCP : tokens) = succeed [] tokens
parseForms tokens =
   [(f:fs, rest) | (f,ys) <- parseForm tokens,
                   (fs,rest) <- parseForms ys ]

parseImpl :: Parser Token Form
parseImpl (TokenImpl : tokens) =
  [ (f,ys) | (f,y:ys) <- parseForm tokens, y == TokenCP ]
parseImpl tokens = []

parseEquiv :: Parser Token Form
parseEquiv (TokenEquiv : tokens) =
  [ (f,ys) | (f,y:ys) <- parseForm tokens, y == TokenCP ]
parseEquiv tokens = []

parse :: String -> [Form]
parse s = [ f | (f,_) <- parseForm (lexer s) ]

arrowfree :: Form -> Form
arrowfree (Prop x) = Prop x
arrowfree (Neg f) = Neg (arrowfree f)
arrowfree (Cnj fs) = Cnj (map arrowfree fs)
arrowfree (Dsj fs) = Dsj (map arrowfree fs)
arrowfree (Impl f1 f2) =
  Dsj [Neg (arrowfree f1), arrowfree f2]
arrowfree (Equiv f1 f2) =
  Dsj [Cnj [f1', f2'], Cnj [Neg f1', Neg f2']]
  where f1' = arrowfree f1
        f2' = arrowfree f2

nnf :: Form -> Form
nnf (Prop x) = Prop x
nnf (Neg (Prop x)) = Neg (Prop x)
nnf (Neg (Neg f)) = nnf f
nnf (Cnj fs) = Cnj (map nnf fs)
nnf (Dsj fs) = Dsj (map nnf fs)
nnf (Neg (Cnj fs)) = Dsj (map (nnf.Neg) fs)
nnf (Neg (Dsj fs)) = Cnj (map (nnf.Neg) fs)

-- Extras

infix 1 -->

(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

fp :: Eq a => (a -> a) -> a -> a
fp f = until (\ x -> x == f x) f

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

update :: Eq a => (a -> b) -> (a,b) -> a -> b
update f (x,y) = \ z -> if x == z then y else f z

updates :: Eq a => (a -> b) -> [(a,b)] -> a -> b
updates = foldl update

fGcd :: Integer -> Integer -> Integer
fGcd a b = if b == 0 then a else fGcd b (rem a b)

pairs :: [(Integer,Integer)]
pairs = concatMap (\ n -> zip [1..n] (repeat n)) [1..]

decomp :: Integer -> (Integer,Integer)
decomp n0 = decomp' (0,n0) where
    decomp' = until (odd.snd) (\ (m,n) -> (m+1,div n 2))

-- function that does a simple optimization like (p^p)->p
optimize :: Form -> Form
optimize (Prop x) = Prop x
optimize (Neg x) = Neg (optimize x)
optimize (Dsj [x,y]) = if (equiv x y) then (optimize x) else Dsj [optimize x,optimize y]
optimize (Cnj [x,y]) = if (equiv x y) then (optimize x) else Cnj [optimize x,optimize y]
optimize (Impl x y) = Impl (optimize x) (optimize y)
optimize (Equiv x y) = Equiv (optimize x) (optimize y)

equiv :: Form -> Form -> Bool
equiv f1 f2 = (entails f1 f2) && (entails f2 f1)

entails :: Form -> Form -> Bool
entails frm1 frm2 = all (\x -> (evl x frm1) --> (evl x frm2)) vals
    where vals = genVals (nub ((propNames frm1) ++ (propNames frm2)))

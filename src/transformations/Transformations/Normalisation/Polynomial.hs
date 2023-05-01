module Transformations.Normalisation.Polynomial where

import Language.Haskell.Generated.Syntax
import Data.Generics.Uniplate.Direct
import Data.Either
import Data.List (sort, partition, permutations)
import Data.Function
import Control.Arrow
import Data.Map hiding (map, filter, partition, (\\), null, foldr)

type ProductTerm = (Int, Expr)
type Product = [ProductTerm]
type Term = (Int, Product)
type Polynomial = [Term]

type PolyBinOp = Polynomial -> Polynomial -> Polynomial
type PolyUnOp = Polynomial -> Polynomial

type ExprPolyMap = Map Expr Polynomial

zeroPoly :: Polynomial
zeroPoly = []

unitPoly :: Polynomial
unitPoly = [(1, [])]

tidy :: (Eq a, Ord a) => [(Int, a)] -> [(Int, a)]
tidy = sort . filter (not . zeroCoeff) . map (foldl1 g) . groupOnSecond
    where g (a, x) (b, _) = (a + b, x)
          zeroCoeff = (== 0) . fst
          groupOnSecond [] = []
          groupOnSecond (x@(_, sec) : rest) =
              uncurry (:) $ ((x :) *** groupOnSecond) $ partition ((== sec) . snd) rest

tidyPoly :: Polynomial -> Polynomial
tidyPoly = tidy . map (second tidy)

-- Operators involved in polynomials:
-- Binary +
-- Binary -
-- Binary *
-- Binary ^

binOpNames :: [String]
binOpNames = ["+", "-", "*", "^"]

isPolyOp :: Expr -> Bool
isPolyOp (App (Con (Operator opName)) _) = elem opName binOpNames
isPolyOp (InfixApp (JustExpr _) (Con (Operator opName)) (JustExpr _)) =
    elem opName binOpNames
isPolyOp _ = False

opAndArgs :: Expr -> Maybe (String, [Expr])
opAndArgs (App (Con (Operator opName)) args@[_, _]) =
    if elem opName binOpNames
       then Just (opName, args)
       else Nothing
opAndArgs (InfixApp (JustExpr p) (Con (Operator opName)) (JustExpr q)) =
    if elem opName binOpNames
       then Just (opName, [p, q])
       else Nothing
opAndArgs _ = Nothing

polyAdd :: PolyBinOp
polyAdd p q = tidyPoly (p ++ q)

polyNegate :: PolyUnOp
polyNegate = map $ first negate

polySubtract :: PolyBinOp
polySubtract p q = polyAdd p $ polyNegate q

polyProduct :: PolyBinOp
polyProduct p q = tidy [mulTerm pTerm qTerm | pTerm <- p, qTerm <- q]
    where mulTerm (pCoeff, pProdTerms) (qCoeff, qProdTerms)
              = (pCoeff * qCoeff, tidy (pProdTerms ++ qProdTerms))

-- p^(-n) = (p^n)^(-1) for n < 0
-- p^0 = 1
-- p^1 = p
-- p^n = p * ... * p (n ps)
polyIntPower :: Polynomial -> Int -> Polynomial
polyIntPower p n | n < 0  = exprToPoly $ exprExprPower (polyToExpr $ polyIntPower p (-n)) $ lint (-1)
                 | n == 0 = unitPoly
                 | n == 1 = p
                 | n > 1  = polyProduct p $ polyIntPower p (n - 1)
polyIntPower _ _ = error "Polynomial: the impossible has happend!" 

polyPower :: PolyBinOp
polyPower _ [] = unitPoly
polyPower p ((i, prodTerms) : rest) = 
    case rest of
      [] -> pResult
      _ -> polyProduct pResult $ polyPower p rest
    where pToI = polyIntPower p i
          {-prodTermsExpr = prodTermProd prodTerms-}

          prodTermsPolyExpr = polyToExpr [(1, prodTerms)]
          pToIExpr = polyToExpr pToI

          pResult = if null prodTerms
                       then pToI
                       else exprToPoly $ exprExprPower pToIExpr prodTermsPolyExpr

intToPoly :: Int -> Polynomial
intToPoly i = if (i == 0) then [] else [(i, [])]

litOne :: Expr
litOne = Lit $ LInt 1

litZero :: Expr
litZero = Lit $ LInt 0

exprIntPower :: Expr -> Int -> Expr
exprIntPower _ 0 = litOne
exprIntPower e 1 = e
exprIntPower e deg = App (Var $ Operator "^") [e, Lit $ LInt deg]

exprExprPower :: Expr -> Expr -> Expr
exprExprPower e f = App (Var $ Operator "^") [e, f]

exprMultiply :: Expr -> Expr -> Expr
exprMultiply e f | e == litOne = f
                 | f == litOne = e
                 | e == litZero = e
                 | f == litZero = f
                 | otherwise = App (Var $ Operator "*") [e, f]

exprAdd :: Expr -> Expr -> Expr
exprAdd e f | e == litZero = e
            | f == litZero = f
            | otherwise = App (Var $ Operator "+") [e, f]

exprToPoly :: Expr -> Polynomial
exprToPoly (Lit (LInt i)) = [(i,[])]
exprToPoly other = [(1, [(1, other)])]

prodTermProd :: Product -> Expr
prodTermProd [] = Lit $ LInt 1
prodTermProd ((deg, expr) : []) = exprIntPower expr deg
prodTermProd ((deg, expr) : rest) =
    exprMultiply (exprIntPower expr deg) $ prodTermProd rest

polyToExpr :: Polynomial -> Expr
polyToExpr = addProducts
    where termToExpr (coeff, expr) = exprMultiply (Lit $ LInt coeff) $ prodTermProd expr

          addProducts [] = litZero
          addProducts (term : []) = termToExpr term
          addProducts (term : rest) = exprAdd (termToExpr term) $ addProducts rest

opNameToBinOp :: String -> Maybe PolyBinOp
opNameToBinOp "+" = Just polyAdd
opNameToBinOp "-" = Just polySubtract
opNameToBinOp "*" = Just polyProduct
opNameToBinOp "^" = Just polyPower
opNameToBinOp _ = Nothing

descendAndCombine :: Expr -> Either Expr Polynomial
descendAndCombine expr = 
    case opAndArgs expr of
      Just (opName, args) -> Right $ process opName args
      Nothing -> Left $ descend normalise expr

    where process opName args = binOp p q
              where (exprs, polys) = partitionEithers $ map descendAndCombine args
                    Just binOp = opNameToBinOp opName
                    [p, q] = map exprToPoly exprs ++ polys                    


normalise :: Expr -> Expr
normalise expr = case descendAndCombine expr of
  Left e     -> e
  Right poly -> polyToExpr poly

-- Equivalence

isHole :: Expr -> Bool
isHole (Hole _) = True
isHole _ = False

divEachOther :: Int -> Int -> Bool
divEachOther m n = (mod m n == 0) || (mod n m == 0)

-- a and b are of the same length and there is a permutation
-- of b so that when zipped with a, all elements are equivalent
-- by eq

fitWithEquiv :: (a -> a -> Bool) -> [a] -> [a] -> Bool
fitWithEquiv eq a b = on (==) length a b && (any (and . zipWith eq a) $ permutations b)

-- Two polynomials are equivalent if:
--- if solP and userQ are of the same length
---- if userQ has no terms whose product terms contain a hole
----- if tidy solP == tidy userQ
---- if userQ has terms whose product terms contain a hole
----- if every product term that is a hole, is raised to the first power
------ there is a permutation perm of userQ such that P a b for (a, b) in zip solP perm
--- otherwise false

-- Terms are of form (Int, [(Int, Expr)])
-- Equivalence of terms:

-- P (c, e) (d, f) is true:
-- if e and f are of equal length
--- if f contains a hole term
---- if all hole terms have power 1
----- if c divides d or d divides c
------ if there is a permutation qerm of f such that Q h i for (h, i) in zip e qerm
--- if f contains no hole term
---- if coefficient c == coefficient d && tidy e == tidy f
--- otherwise false

-- Product terms are of form (Int, Expr)
-- Equivalence of product terms:

-- Q h i@(iPower, expr) is true:
-- if expr matches (Hole _)
--- if power == 1
-- if h == i
-- otherwise false

polyEquiv :: Polynomial -> Polynomial -> Bool
polyEquiv solP userQ = tidy solP == tidy userQ || fitWithEquiv termMatch solP userQ
    where termMatch p q = if termHasHole q
                          then coeffsMatch && holePowersOkay && fitWithEquiv prodTermMatch e f
                          else on (==) (second tidy) p q
              where termHasHole = any (isHole . snd) . snd

                    (c, e) = p
                    (d, f) = q       

                    coeffsMatch = divEachOther c d

                    prodTermOkay (1, Hole _) = True
                    prodTermOkay (_, Hole _) = False
                    prodTermOkay _ = True

                    holePowersOkay = all prodTermOkay f

                    prodTermMatch _ (1, Hole _) = True
                    prodTermMatch v w = v == w

polyPrint :: Polynomial -> String
polyPrint [] = "0"
polyPrint p = foldr addStr "" $ map printTerm p
 where 
  {-addTerm term acc = addStr (printTerm term) acc-}
          
  printTerm (1, prod) = printProduct prod
  printTerm (i, prod) = show i ++ printProduct prod

  printProduct = foldr multiplyStr "" . map printProdTerm
          
  printProdTerm (1, expr) = printExpr expr
  printProdTerm (d, expr) = printExpr expr ++ "^" ++ show d

  printExpr (Var (Ident x)) = x
  printExpr expr = "(" ++ show expr ++ ")"

  addStr s "" = s
  addStr s t = s ++ " + " ++ t

  multiplyStr s "" = s
  multiplyStr s t = s ++ "*" ++ t

-- Examples

lint :: Int -> Expr
lint = Lit . LInt

var :: String -> Expr
var = Var . Ident

{-p :: Polynomial-}
{-p = [(3, [(2, var "x"), (1, var "y")]), (5, [(1, var "x")])]-}

{-q :: Polynomial-}
{-q = [(5, []), (4, [(1, var "x"), (1, var "y")])]-}

{-r :: Polynomial-}
{-r = [(3, [(2, var "x"), (3, var "y")])]-}

{-s :: Polynomial-}
{-s = [(3, [(3, var "y"), (2, var "x")]), (5, [(1, var "z")])]-}

{-t :: Polynomial-}
{-t = [(3, [(3, var "y"), (1, Hole 1)]), (10, [(1, Hole 2)])]-}

{-u :: Polynomial-}
{-u = [(3, [(3, var "y"), (2, Hole 1)])]-}

{-v :: Polynomial-}
{-v = [(3, [(3, var "y"), (1, var "z"), (1, Hole 1)])]-}

{-w :: Polynomial-}
{-w = [(3, [(2, var "x"), (3, var "y")]), (1, [(1, var "z")])]-}

{-x :: Polynomial-}
{-x = [(3, [(2, var "x"), (3, var "y")]), (1, [(1, Hole 1)])]-}

module Transformations.Normalisation.Boolean
    ( normaliseBooleanExprs
    , negateNormalizedBooleanExpr
    ) where

import Language.Haskell.Generated.Syntax
import Transformations.Normalisation.Boolean.DecTree

import Data.Generics.Uniplate.Direct

-- | Deriving a decision tree element from an expression. The boolean indicates whether this element
--   should be negated.
elementFromExpr :: Expr -> (Element, Bool)
elementFromExpr expr = case expr of
    App (InfixApp NoExpr (Var (Operator op)) NoExpr) [a, b] -> binOp op a b
    App (InfixApp (JustExpr a) (Var (Operator op)) NoExpr) [b] -> binOp op a b
    App (InfixApp NoExpr (Var (Operator op)) (JustExpr b)) [a] -> binOp op a b
    InfixApp (JustExpr a) (Var (Operator op)) (JustExpr b) -> binOp op a b
    Paren e -> elementFromExpr e
    _ -> (Atom expr, False)
  where
    binOp op a b =
        case op of
            "==" -> (EqualRelation a b, False)
            "/=" -> (EqualRelation a b, True)
            "<" -> (LessThanRelation a b, False)
            ">" -> (LessThanRelation b a, False)
            ">=" -> (LessThanRelation a b, True)
            "<=" -> (LessThanRelation b a, False)
            _ -> (Atom expr, False)

-- | If Expr is an application of a boolean operator/function (&&, ||, not), then this derives a
--   decision tree from it. Otherwise, Nothing is returned.
treeFromExpr :: Expr -> Maybe DecTree
treeFromExpr expr = case expr of
    App (InfixApp NoExpr op NoExpr) [a, b] -> binOp op a b
    App op [a] -> unOp op a
    InfixApp (JustExpr a) op (JustExpr b) -> binOp op a b
    Paren e -> treeFromExpr e
    _ -> Nothing
  where
    unOp (InfixApp (JustExpr a) op NoExpr) b = binOp op a b
    unOp (InfixApp NoExpr op (JustExpr b)) a = binOp op a b
    unOp (Var (Ident "not")) x = Just . negateTree . treeFromExpr' $ x
    unOp _ _ = Nothing
    binOp (Var op) a b = do
        op' <- booleanOp op
        let a' = treeFromExpr' a
            b' = treeFromExpr' b
        return $ combineTrees op' a' b'
    binOp _ _ _ = Nothing
    booleanOp op = case op of
        Operator "&&" -> Just BAnd
        Operator "||" -> Just BOr
        _ -> Nothing

-- | Treats non-matching expressions as elements and wraps them with singleNode.
treeFromExpr' :: Expr -> DecTree
treeFromExpr' e = case treeFromExpr e of
    Just t -> t
    Nothing ->
        let (el, neg) = elementFromExpr e
        in  (if neg then negateTree else id) $ singleNode el

treeToNormalisedExpr :: DecTree -> Expr
treeToNormalisedExpr = treeToNormalisedExpr' . normaliseTree

treeToNormalisedExpr' :: DecTree -> Expr
treeToNormalisedExpr' (DLeaf b)
    | b = Con $ Ident "True"
    | otherwise = Con $ Ident "False"
treeToNormalisedExpr' (DNode l el r) =
    let expr = elementToExpr el
    in  case (l, r) of
            -- Note: the specialized cases are not absolutely neccessary, but cause the output to be
            -- somewhat nicer looking (i.e. contain a little less redundancy).
            (DLeaf True, DLeaf False) -> expr
            (DLeaf False, DLeaf True) -> notOp expr
            (_, DLeaf False) -> andOp expr (treeToNormalisedExpr' l)
            (DLeaf True, _) -> orOp expr (treeToNormalisedExpr' r)
            _ ->
                orOp
                    (andOp expr $ treeToNormalisedExpr' l)
                    (andOp (notOp expr) $ treeToNormalisedExpr' r)
  where
    andOp = makeBinOp "&&"
    orOp = makeBinOp "||"
    notOp x = App (Var $ Ident "not") [Paren x]

makeBinOp :: String -> Expr -> Expr -> Expr
makeBinOp op a b = App (InfixApp NoExpr (Var (Operator op)) NoExpr) [Paren a, Paren b]

elementToExpr :: Element -> Expr
elementToExpr el =
    case el of
        Atom e -> e
        EqualRelation a b -> makeBinOp "==" a b
        LessThanRelation a b -> makeBinOp "<" a b

-- | Descends an expression in a top-down manner, normalising boolean expressions if encountered.
normaliseBooleanExpr :: Expr -> Expr
normaliseBooleanExpr expr = case normaliseBooleanExpr' expr of
    Just norm -> norm
    Nothing -> descend normaliseBooleanExpr expr

-- | If Expr represents a boolean operation, it is normalized and atoms are being recursed into.
--   Otherwise, Nothing is returned.
normaliseBooleanExpr' :: Expr -> Maybe Expr
normaliseBooleanExpr' e =
    do
        tree <- treeFromExpr e
        let tree' = transformAtoms normaliseBooleanExpr tree
        return $ treeToNormalisedExpr tree'

-- | Perform the normalisation on a module, in a top-down manner.
normaliseBooleanExprs :: Module -> Module
normaliseBooleanExprs = descendBi normaliseBooleanExpr

-- | Negate the atoms within a previously normalised boolean expression.
negateNormalizedBooleanExpr :: Expr -> Expr
-- TODO: do not reconstruct tree but utilise fact that expression is already normalised to simply
-- remove and add 'not' applications in the right places.
-- negateNormalizedBooleanExpr = treeToNormalisedExpr . negateTree . treeFromExpr'
negateNormalizedBooleanExpr expr =
    case expr of
        App (InfixApp NoExpr (Var (Operator "&&")) NoExpr) [Paren a, Paren b] ->
            App
                (InfixApp NoExpr (Var (Operator "||")) NoExpr)
                [Paren $ negateNormalizedBooleanExpr a, Paren $ negateNormalizedBooleanExpr b]
        App (InfixApp NoExpr (Var (Operator "||")) NoExpr) [Paren a, Paren b] ->
            App
                (InfixApp NoExpr (Var (Operator "&&")) NoExpr)
                [Paren $ negateNormalizedBooleanExpr a, Paren $ negateNormalizedBooleanExpr b]
        App (Var (Ident "not")) [Paren x] -> x
        x -> App (Var (Ident "not")) [Paren x]

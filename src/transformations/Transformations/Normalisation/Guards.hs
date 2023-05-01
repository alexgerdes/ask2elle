-----------------------------------------------------------------------------
-- Copyright 2014, Open Universiteit Nederland. This file is distributed
-- under the terms of the GNU General Public License. For more information,
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Maintainer  :  alex@botkes.nl
-- Stability   :  provisional
-- Portability :  unknown
module Transformations.Normalisation.Guards (normaliseTotalGuards) where

import Data.Generics.Uniplate.Direct
import Language.Haskell.Generated.Syntax

-- | Transforms 'total' sets of guards (i.e. those of which the last case is 'otherwise' or 'True')
--   into equivalent if-statements.
normaliseTotalGuards :: Module -> Module
normaliseTotalGuards = transformBi normaliseRhs

-- | Attempts performing the normalisation on the right-hand-side of an expression. Does not
--   recurse into child expressions, so apply this in a bottom-up manner.
normaliseRhs :: Rhs -> Rhs
normaliseRhs (GRhs guards decls) | isTotal guards = Rhs (guardsToIf guards) decls
normaliseRhs rhs = rhs

-- | Indicates whether a list of guarded expressions is certain to be total.
--   This is the case when the final guard condition is either 'otherwise' or 'True'. Other forms
--   of total guards are either not very sensical (e.g. when the final condition is a tautology) or
--   would require a more extensive analysis (e.g. when the conditions are 'a >= b' and 'a < b').
isTotal :: GuardedExprs -> Bool
isTotal guards = finalExp `elem` [Con $ Ident "True", Var $ Ident "otherwise"]
  where
    GExpr finalExp _ = last guards

-- | Transforms a set of total guarded expressions into an equivalent if-then-else expression.
guardsToIf :: GuardedExprs -> Expr
guardsToIf [GExpr _ expr] = expr
guardsToIf (GExpr cond expr : rest) = If cond expr (guardsToIf rest)
guardsToIf [] =
    error $
        "Language.Haskell.Transformations.Normalisation.Guards.guardsToIf: empty list "
            ++ "of guarded expressions."

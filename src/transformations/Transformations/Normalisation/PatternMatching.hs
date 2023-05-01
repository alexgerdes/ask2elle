module Transformations.Normalisation.PatternMatching (normaliseCaseMatchOrders) where

import Language.Haskell.Generated.Syntax

import Control.Arrow
import Control.Monad
import Data.Generics.Uniplate.Direct
import Data.List
import Data.Maybe

-- | When it is noted that it is possible to reorder the different pattern matches within a
--   case-expression without changing the semantics, this will sort them using some consistant
--   order.
normaliseCaseMatchOrders :: Module -> Module
normaliseCaseMatchOrders = transformBi normaliseCaseMatchOrders'
  where
    normaliseCaseMatchOrders' (Case expr alts) = Case expr $ normaliseCaseMatchOrder alts
    normaliseCaseMatchOrders' expr = expr

-- | Performs a simple lexicograpgic sort on case alternatives only if it is certain that they are
--   independent: i.e. the patterns do not overlap.
normaliseCaseMatchOrder :: Alts -> Alts
normaliseCaseMatchOrder alts
    | length alts > 1 && independent alts = sort alts
    | otherwise = alts

-- | Checks whether a list of patterns certainly does not overlap. It does this by simply fetching
--   the top constructor (or the value of a literal, if no contructor is presen) in each match and
--   checking whether no constructor is used more than once.
--   Quite some cases, such as the pattern sets {(0:xs),(1:xs)} or {(True,False),(False,True)}, will
--   result in a false negative. Common and simple cases such as {[],(x:xs)} will be detected
--   though.
independent :: Alts -> Bool
independent = mapM topCtor >>> liftM (not . hasDups) >>> fromMaybe False
  where
    hasDups (x : xs) = x `elem` xs || hasDups xs
    hasDups [] = False
    constructor = Just . Left
    literal = Just . Right
    noCons = Nothing

    topCtor :: Alt -> Maybe (Either Name Literal)
    topCtor (Alt _ pat _) = patCtor pat
    topCtor _ = noCons
    patCtor pat =
        case pat of
            PAs _ p -> patCtor p
            PCon con _ -> constructor con
            PInfixCon _ con _ -> constructor con
            PList [] -> constructor $ Special "[]"
            PList _ -> constructor $ Operator ":"
            PLit lit -> literal lit
            PParen p -> patCtor p
            PTuple [] -> constructor $ Special "()"
            PTuple [p] -> patCtor p
            PTuple _ -> noCons
            PVar _ -> noCons
            PWildcard -> noCons
            PHole _ -> noCons
            PMultipleHole _ -> noCons

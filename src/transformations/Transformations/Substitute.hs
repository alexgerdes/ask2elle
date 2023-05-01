{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- Copyright 2014, Open Universiteit Nederland. This file is distributed 
-- under the terms of the GNU General Public License. For more information, 
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------
-- |
-- Maintainer  :  alex@botkes.nl
-- Stability   :  provisional
-- Portability :  unknown
--
--
-----------------------------------------------------------------------------

module Transformations.Substitute 
   ( Subst, substitute, patternMatch
   , insertSubst, emptySubst, singletonSubst
   , toList, unionSubst, mapSubst
   ) where

import Prelude hiding (lookup)

import Control.Monad
import Data.Generics.Uniplate.Direct
import qualified Data.Map as M
import Language.Haskell.Generated.Syntax


newtype Subst = S { unS :: M.Map Name Expr } deriving Show

-- | After compilation/typchecking, so we know for sure that pat ~= expr
-- could use substMapM, but I want to see where no subst can be made
substitute :: (Biplate a Expr) => Subst -> a -> a
substitute sub = transformBi f
 where
   f e@(Var n) = M.findWithDefault e n (unS sub)
   f e         = e

patternMatch :: Pat -> Expr -> Maybe Subst
patternMatch p e = 
   case (p, e) of
      -- first remove parens
      (PParen p', expr) -> patternMatch p' expr
      (pat, Paren e') -> patternMatch pat e'
      -- build map with substitutions
      (PVar n, e') -> Just (singletonSubst n e')
      (PCon _ ps, Con _ `App` es) -> matchList ps es               
      (PWildcard, _) -> Just emptySubst
      (PTuple ps, Tuple es) -> matchList ps es
      (PList ps, List es) -> do
         guard (length ps == length es) 
         matchList ps es
      (PLit l, e') -> do 
         guard (Lit l == e') 
         return emptySubst
      _ -> Nothing
   where
      matchList ps es = liftM (S . M.unions . map unS) $ zipWithM patternMatch ps es

emptySubst :: Subst
emptySubst = S M.empty

unionSubst :: Subst -> Subst -> Subst
unionSubst (S m) (S m') = S $ M.union m m'

mapSubst :: (Expr -> Expr) -> Subst -> Subst
mapSubst f =  S . M.map f . unS

singletonSubst :: Name -> Expr -> Subst
singletonSubst n a = S (M.singleton n a)

insertSubst :: Name -> Expr -> Subst -> Subst
insertSubst n a = S . M.insert n a . unS

toList :: Subst -> [(Name, Expr)]
toList = M.toList . unS



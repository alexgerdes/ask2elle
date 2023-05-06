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
--   rewrite... = write AS in a different form
--   remove...  = remove parts of AST
--
-----------------------------------------------------------------------------

module Transformations.Desugar where

import Language.Haskell.Generated.Syntax
import Language.Haskell.Utility.SyntaxInstances
import Language.Haskell.Utility.Utils (freshNames)

import Data.Generics.Uniplate.Direct
import Control.Monad.State
import Data.List
import Data.Functor.Identity (Identity)

instance MonadFail Identity where  
   fail = error

-- Todo:
-- 
-- * add removal of as-patterns

-- | Remove syntatic sugar
desugar :: Module -> Module
desugar = rewriteWhere          -- rewrite where to let
        . removePatParens       -- remove pattern parentheses
        . removeExprParens      -- remove expression parentheses
        . rewriteLambda         -- remove sugar: \x y -> x  =>  \x -> \y -> x
        . rewriteInfix          -- e.g., 1 + 2  =>  (+) 1 2
--        . removePatBind         -- e.g., f = let g = ... in g  =>  f = ...
        . removeApps            -- e.g., ((f 1) 2) = f 1 2
        . removeExplicitApps    -- remove $
--        . rewriteWildcards


-- rewrite lambda Exprs, e.g., \x y -> x + y => \x -> \y -> x + y
rewriteLambda :: BiplateFor a => a -> a
rewriteLambda = transformBi $ \ expr ->
   case expr of
      Lambda (x:y:ys) e -> Lambda [x] $ Lambda (y:ys) e
      _ -> expr

removeExprParens :: Module -> Module
removeExprParens = transformBi $ \ expr ->
   case expr of
      Paren expr' -> expr'
      _           -> expr

removePatParens :: Module -> Module
removePatParens = transformBi $ \ pat ->
   case pat of
      PParen pat' -> pat'
      _           -> pat

-- | application rewrites, e.g. ((f 1) 2) 3 => f 1 2 3
removeApps :: Module -> Module
removeApps = transformBi $ \ expr ->
   case expr of
      (f `App` args) `App` args' -> f `App` (args ++ args')
      f `App` []                 -> f
      _                          -> expr

-- infix application rewrites, e.g. 1 `div` 2 => div 1 2 or 1 + 2 => (+) 1 2
rewriteInfix :: Module -> Module
rewriteInfix = transformBi $ \ expr ->
   case expr of
      InfixApp l f r ->
         case (l, r) of
            (NoExpr, NoExpr)          -> expr
            (JustExpr x,  NoExpr)     -> op f `App` [x]
            (NoExpr, JustExpr x)      -> op f `App` [x]
            (JustExpr x,  JustExpr y) -> op f `App` [x, y]
      _                               -> expr
   where
      op f = InfixApp NoExpr f NoExpr

-- quick'n dirty domain specific rewrite rules
-- (+) 2 1   =>  (+) 1 2
rewriteCommutativeOps :: Module -> Module
rewriteCommutativeOps = transformBi $ \ expr ->
   case expr of
      InfixApp NoExpr f NoExpr `App` args 
         | not (isSorted args) && (isOp f "+" || isOp f "*") ->
            InfixApp NoExpr f NoExpr `App` sort args
      _ -> expr
   where
      isSorted xs = sort xs == xs
      isOp op n =
         case op of
            Var (Operator n') -> n == n'
            _                 -> False

-- f = let g x = x+ 1 in g   =>   g x = x + 1
-- removePatBind :: Module -> Module
-- removePatBind = transformBi $ \ decl ->
--    case decl of
--       DPatBind p (Rhs (Let [d] (Var _)) []) -> d
--       _                                     -> decl

-- g = f $ 1   =>   g = f 1
removeExplicitApps :: Module -> Module
removeExplicitApps = transformBi $ \ expr ->
   case expr of
      InfixApp (JustExpr f) (Var (Operator "$")) (JustExpr arg) 
         -> f `App` [arg]
      _  -> expr

-- | Rewrite wheres to lets:   f = g where g = 1   =>   f = let g = 1 in g
rewriteWhere :: Module -> Module
rewriteWhere = transformBi $ \ rhs ->
   case rhs of
      Rhs expr ds | not (null ds) -> Rhs (Let ds expr) []
      _                           -> rhs

rewriteWildcards :: Module -> Module
rewriteWildcards x = evalState (transformBiM f x) (freshNames x 'w')
   where
      f PWildcard = do
         (n:ns) <- get
         put ns
         return $ PVar n
      f p = return p

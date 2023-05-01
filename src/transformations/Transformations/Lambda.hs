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
--
-- Lambda calculus transformations
module Transformations.Lambda
    ( etaReduce
    , betaReduce
    , reduce
    , alphaRename
    , alphaRenameWithMap
    , alphaRenameCtx
    , NameEnv
    ) where

import Control.Monad
import Data.Generics.Uniplate.Direct
import qualified Data.Map as M
import Data.Maybe
import Ideas.Common.Library
import Language.Haskell.Generated.Syntax
import Language.Haskell.Utility.PrettyPrint
import Language.Haskell.Utility.SyntaxInstances

-- import Language.Haskell.Transformations.AlphaRenaming

import Language.Haskell.Utility.Utils (noShadow)
import qualified Transformations.Alpha as Alpha
import Transformations.Substitute

type NameEnv = M.Map Name Name

-- | preform lambda calculus reductions
reduce :: BiplateFor a => a -> a
reduce = rewriteBi $ betaReduce >-> etaReduce

-- |eta reduction, e.g., \x -> f x => f
etaReduce :: Expr -> Maybe Expr
etaReduce (Lambda [PVar x] (f `App` args)) =
    case last args of
        Var v -> do
            guard (x == v)
            return $ f `App` init args
        _ -> Nothing
etaReduce (App f []) = Just f
etaReduce _ = Nothing

-- beta reduction, e.g.,
-- (\x -> x + x) a   =>   a + a
-- (\(a, b) -> a + b) (1, 2)   =>   1 + 2
betaReduce :: Expr -> Maybe Expr
betaReduce e =
    case e of
        Lambda [pat] expr `App` (arg : args) -> do
            sub <- patternMatch pat arg
            let expr' = substitute sub expr
            return $ if null args then expr' else expr' `App` args
        _ -> Nothing

-- | alpha renaming
-- f x = x + 1   =>   x1 x2 = x2 + 1
alphaRenameWithMap :: [Name] -> Module -> (Module, NameEnv)
alphaRenameWithMap fixed m =
    let ai = Alpha.alphaInfo fixed m in checkShadowing (Alpha.result ai, Alpha.invSubst ai)
  where
    checkShadowing (x, nm)
        | noShadow x = (x, nm)
        | otherwise =
            error $
                "Alpha renaming failed: "
                    ++ show m
                    ++ "\n\n"
                    ++ pprint m
                    ++ "\n\n"
                    ++ pprint x
                    ++ "\n\n"
                    ++ show nm
                    ++ "\n\n"
                    ++ show fixed

alphaRename :: [Name] -> Module -> Module
alphaRename fixedNames = fst . alphaRenameWithMap fixedNames

alphaRenameCtx :: Context Module -> Context Module
alphaRenameCtx ctx = applyTop (alphaRename fs) ctx
  where
    mfs = makeRef "fixedNames" ? ctx
    fs = fromMaybe (error "Alpha renaming from context failed!") mfs

-- | Help functions

-- | transformation combinators
(>->) :: (a -> Maybe a) -> (a -> Maybe a) -> a -> Maybe a
(f >-> g) x = f x `mplus` g x

infixr 1 >->

-- (>>->) :: (Data a, Data b) => (a -> Maybe a) -> (b -> Maybe b) -> Module -> Maybe Module
-- (f >>-> g) x = liftTrans f x `mplus` liftTrans g x
-- infixr 2 >>->
--
-- -- Choice do all rewrites in a Module or just one and let it to the rewriteBi
-- liftTrans :: (Data a, Data b) => (a -> Maybe a) -> b -> Maybe b
-- liftTrans rule m =
--    safeHead [ ctx a | (h, ctx) <- contextsBi m, Just a <- [rule h] ]

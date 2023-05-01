{-# LANGUAGE FlexibleContexts #-}

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
module Transformations.Preprocess
    ( rewriteRecurPatBind
    , rewriteConstantArgs
    , rewriteFbsCase
    , removeHoles
    , preprocess
    ) where

import Control.Monad.State hiding (fix)
import Data.Generics.Uniplate.Direct
import Data.List (intersect, transpose, (\\))
import qualified Data.Map as M
import Data.Maybe
import Language.Haskell.Generated.Syntax
import Language.Haskell.Utility.SyntaxInstances
import Language.Haskell.Utility.Utils
import Transformations.Substitute

import Data.Functor.Identity (Identity)

-- | Preprocess, for example make beta reducible
preprocess :: Module -> Module
preprocess =
    rewriteFbsCase
        --           . rewriteConstantArgs
        . rewriteRecurPatBind

-- | Rewrite function bindings to a pattern binding with a case:
--   f x = 1 ; f (x:xs) = 1 + f xs   =>   f = \x -> case x of x -> 1 ; (x:xs) -> 1 + f xs
rewriteFbsCase :: Module -> Module
rewriteFbsCase m = evalState (transformBiM f m) (freshNames m 'y')
  where
    fb2alt fb =
        case fb of
            FunBind mfb _ ps rhs -> Alt mfb (PTuple ps) rhs
            FBHole i -> AHole i

    f d@(DFunBinds fbs) =
        case [(name, length ps) | FunBind _ name ps _ <- fbs] of
            ((name, n) : _) -> do
                names <- get
                let ids = take n names
                put $ drop n names
                return $
                    DPatBind (PVar name) $
                        Rhs
                            ( Lambda
                                (map PVar ids)
                                ( Case
                                    (Tuple $ map Var ids)
                                    (map fb2alt fbs)
                                )
                            )
                            []
            _ -> return d
    f d = return d

-- | Rewrite a recursive pattern binding to a fix. An alternative is to write
--   it as a let. Afterwards the pat. binding can be inlined. For example:
--
--   f = 1 : f           =>   f = fix (\g -> 1 : g)
--   f = \ x -> x : f x  =>   f = fix (\g -> (\x -> x : g x))
--
--   Alternative:  f = 1 : f   =>   f = let f = 1 : f in f
rewriteRecurPatBind :: Module -> Module
rewriteRecurPatBind m = evalState (transformBiM g m) (freshNames m 'f')
  where
    g decl@(DPatBind p (Rhs expr ds)) | isRecursive decl = do
        (n : ns) <- get
        let msub = patternMatch p (Var n)
            ds' = maybe ds (\sub -> map (substitute sub) ds) msub
            expr' = maybe expr (`substitute` expr) msub
            fix = Var $ Ident "fix"
        put ns
        return $ DPatBind p (Rhs (fix `App` [Paren $ Lambda [PVar n] expr']) ds')
    g d = return d

-- | If a function has invariant arguments (arguments that are passed unchanged
--   around in a recursive function), rewrite the function by abstracting away
--   the invariant arugments. The resulting declaration becomes beta reducible.
--   E.g.:
--
--   f c []     = []                f = \ n -> let f []     = []
--   f c (x:xs) = c : f c xs   =>                  f (x:xs) = c : f xs
--                                             in f

-- FIXME: only the first argument(s) are allowed, otherwise the interface of
-- the function will change (eg, f [] n = [] -> f =\ n -> [] = [])
-- simplify the function to operate only on the first arg and call recusively
-- on the rest
rewriteConstantArgs :: Module -> Module
rewriteConstantArgs m
    | noShadow m = evalState (transformBiM rew m) (freshNames m 'f')
    | otherwise = error "Preprocess: const args, shadowing!"
  where
    rew decl = do
        (f : fs) <- get
        put fs
        case decl of
            DFunBinds fbs@(FunBind _ n ps _ : _) | isRecursive decl ->
                case invArgPos fbs of
                    [] -> return decl
                    is ->
                        return $
                            DPatBind (PVar n) $
                                Rhs
                                    ( Lambda (elemsAt is ps) $
                                        Let
                                            [DFunBinds $ map (rewriteFB f is) fbs]
                                            (Var f)
                                    )
                                    []
            _ -> return decl

    invArgPos = foldr1 intersect . map invArgs

    rewriteFB f is fb =
        case fb of
            FunBind mfb n ps rhs -> FunBind mfb f (deleteAt is ps) $ rewriteApps f n is rhs
            _ -> fb

    -- \| Remove invariant args (by index) of every function call n
    rewriteApps :: BiplateFor a => Name -> Name -> [Int] -> a -> a
    rewriteApps f n indices = transformBi $ \x ->
        case x of
            Var n' `App` as
                | n == n' ->
                    Var f `App` M.elems (foldr M.delete (M.fromList $ zip [0 ..] as) indices)
            _ -> x

    -- f n x = f n 2 + f n 3 => [0]
    invArgs :: FunBind -> [Int]
    invArgs fb =
        case fb of
            FBHole _ -> []
            FunBind _ n ps rhs ->
                if null ass
                    then [0 .. length ps - 1]
                    else mapMaybe g $ zip3 ps ass [0 ..] -- return invariant args indices
              where
                g (p, as, i)
                    | all (maybe False (all (\(a, b) -> Var a == b) . toList) . patternMatch p) as = Just i
                    | otherwise = Nothing
                ass =
                    transpose [args | Var n' `App` args <- universeBi rhs, n == n'] -- get all function call arguments

instance MonadFail Identity where
    fail = error

-- | Remove superfluous holes from lists, such as decls in a where clause
removeHoles :: Module -> Module
removeHoles = transformBi f . transformBi j . transformBi g . transformBi h
  where
    noDeclHoles =
        mapMaybe
            ( \x -> case x of
                DHole _ -> Nothing
                DFunBinds [] -> Nothing
                _ -> Just x
            )
    noFBHoles = mapMaybe (\x -> case x of FBHole _ -> Nothing; _ -> Just x)
    noAltHoles = mapMaybe (\x -> case x of AHole _ -> Nothing; _ -> Just x)

    f (Body ds) = Body $ noDeclHoles ds
    f x = x
    g (DFunBinds fbs) = DFunBinds $ noFBHoles fbs
    g x = x
    h (Let ds e) = Let (noDeclHoles ds) e
    h (Case e as) = Case e $ noAltHoles as
    h x = x
    j (Rhs e ds) = Rhs e $ noDeclHoles ds
    j x = x

-- | Help functions
elemsAt :: Eq a => [Int] -> [a] -> [a]
elemsAt is xs = xs \\ deleteAt is xs

-- | delete elements at given positions
deleteAt :: [Int] -> [a] -> [a]
deleteAt = flip (foldr f)
  where
    f i xs
        | i < length xs = let (ys, zs) = splitAt i xs in ys ++ tail zs
        | otherwise = xs

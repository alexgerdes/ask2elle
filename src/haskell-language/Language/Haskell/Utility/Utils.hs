{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- Copyright 2015, Open Universiteit Nederland. This file is distributed
-- under the terms of the GNU General Public License. For more information,
-- see the file "LICENSE.txt", which is included in the distribution.
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Maintainer  :  alex@botkes.nl
-- Stability   :  provisional
-- Portability :  unknown
module Language.Haskell.Utility.Utils
    ( isRecursive
    , ignoreHoleIDs
    , showName
    , try
    , freshNames
    , fst4
    , fth4
    , liftMaybe
    , liftEither
    , fst3
    , snd3
    , thd3
    , showDiff
    , bindings
    , fromRight
    , safeHead
    , trim
    , containsHoles
    , noShadow
    {-, ppStrategy-}
    , stopOnShadow
    , uniqueRules
    )
where

import Control.Monad.State
import Data.Char
import Data.Generics.Uniplate.Direct
import Data.List
import Data.Maybe
import qualified Data.Traversable as Trav
import Data.Tree

import Ideas.Common.Library hiding (bindings, check, label, try, vars)
import Ideas.Common.Strategy.Abstract (onStrategyTree)

import Language.Haskell.Generated.Syntax
-- import Language.Haskell.Utility.PrettyPrint
import Language.Haskell.Utility.SyntaxInstances

{-import Ideas.Common.Strategy.Core hiding (Let, Var)-}

containsHoles :: BiplateFor a => a -> Bool
containsHoles m =
    not $
        null $
            [() | DHole _ <- universeBi m]
                ++ [() | Hole _ <- universeBi m]
                ++ [() | PHole _ <- universeBi m]
                ++ [() | AHole _ <- universeBi m]
                ++ [() | FBHole _ <- universeBi m]

uniqueRules :: Strategy a -> Strategy a
uniqueRules = onStrategyTree $ flip evalState (0 :: Int) . Trav.mapM rename
  where
    rename r = do
        n <- get
        put (succ n)
        return $ describe (description r) $ changeId (# show n) r

-- | Check whether a declaration has recursive function calls
isRecursive :: Decl -> Bool
isRecursive d
    -- \| not (noShadow d) = error $ "call to isRecursive with shadowing: \n" ++ pprint d
    | otherwise =
        case d of
            DPatBind p rhs -> not $ null $ intersect (pvars p) $ vars rhs
            DFunBinds fbs@(FunBind _ n _ _ : _) -> Var n `elem` concatMap vars fbs
            _ -> False
  where
    vars :: Biplate a Expr => a -> [Expr]
    vars x = [v | v@(Var _) <- universeBi x]
    pvars x = [Var n | (PVar n) <- universeBi x]

noShadow :: BiplateFor a => a -> Bool
noShadow x =
    all d (universeBi x)
        && all f (universeBi x)
        && all e (universeBi x)
        && all r (universeBi x)
  where
    disjoint xs = all (`notElem` xs)
    -- patbind
    d (DPatBind p rhs) = bindings p `disjoint` bindings rhs
    d _ = True
    -- funbind
    f (FunBind _ f' ps rhs) = (f' : concatMap bindings ps) `disjoint` bindings rhs
    --      && f' `notElem` concatMap bindings ps
    f _ = True
    -- let
    e (Let ds e') = all noShadow ds && concatMap topBindings ds `disjoint` bindings e'
    -- lambda
    e (Lambda ps e') = concatMap bindings ps `disjoint` bindings e'
    e _ = True
    -- where
    r (Rhs e' w) = concatMap topBindings w `disjoint` bindings e'
    r (GRhs ges w) = concatMap topBindings w `disjoint` concatMap bindings ges

topBindings :: Decl -> [Name]
topBindings (DPatBind p _) = bindings p
topBindings (DFunBinds (FunBind _ f ps _ : _)) = f : concatMap bindings ps
topBindings _ = []

bindings :: BiplateFor a => a -> [Name]
bindings x =
    nub $
        [n | (PVar n) <- universeBi x]
            ++ [n | (PAs n _) <- universeBi x]
            ++ [n | (FunBind _ n _ _) <- universeBi x]

stopOnShadow :: (Show a, BiplateFor a, PrettyPrint a) => a -> a
stopOnShadow m
    | noShadow m = m
    | otherwise =
        error $
            "preprocess failed: "
                ++ show m
                ++ "\n\n"
                ++ pprint m

ignoreHoleIDs :: (Biplate a Decl, Biplate a Expr, Biplate a Pat) => a -> a
ignoreHoleIDs = transformBi f . transformBi g . transformBi h
  where
    f d = case d of DHole _ -> DHole ""; _ -> d
    g e = case e of Hole _ -> Hole ""; _ -> e
    h p = case p of PHole _ -> PHole ""; _ -> p

showName :: Name -> String
showName n =
    case n of
        Ident s -> s
        Operator s -> s
        Special s -> s

freshNames :: Module -> Char -> [Name]
freshNames m c = [Ident (c : show n) | n <- [(1 :: Int) ..]] \\ ns
  where
    ns = universeBi m :: [Name]

try :: (a -> Maybe a) -> a -> a
try f a = fromMaybe a $ f a

fst4 :: (a, b, c, d) -> a
fst4 (a, _, _, _) = a

fth4 :: (a, b, c, d) -> d
fth4 (_, _, _, d) = d

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, x, _) = x

thd3 :: (a, b, c) -> c
thd3 (_, _, x) = x

fromRight :: b -> Either a b -> b
fromRight x = either (const x) id

showDiff :: [(Tree String, Tree String)] -> IO ()
showDiff = mapM_ f
  where
    line n = putStrLn $ replicate n '-'
    f (t, t') = do
        line 80
        putStrLn $ drawTree t
        putStrLn " =/=\n"
        putStrLn $ drawTree t' ++ "\n"

-- -- from SYB examples
-- treealise :: Data a => a -> Tree String
-- treealise = gdefault `extQ` atString
--    where
--       atString (x::String) = Node x []
--       gdefault x = Node (showConstr (toConstr x)) (gmapQ treealise x)

-- -- De-trealise Tree to Data
-- detreealise :: Data a => Tree String -> Maybe a
-- detreealise = gdefault `extR` atString
--   where
--     atString (Node x []) = Just x
--     atString x = error $ show x
--     gdefault (Node x ts) = res
--       where
--         -- a helper for type capture
--         res  = maybe Nothing (kids . fromConstr) con
--         -- the type to constructed
--         ta   = fromJust res
--         -- construct constructor
--         con  = readConstr (dataTypeOf ta) x
--         -- recursion per kid with accumulation
--         perkid ts' = const (tail ts', detreealise (head ts'))
--         -- recurse into kids
--         kids a =
--           do guard (glength a == length ts)
--              snd (gmapAccumM perkid ts a)

-- printTree :: Data a => a -> IO ()
-- printTree = putStrLn . drawTree . treealise

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

liftEither :: (Show a, Monad m, MonadFail m) => Either a b -> m b
liftEither = either (fail . show) return

liftMaybe :: (Monad m, MonadFail m) => Maybe a -> m a
liftMaybe = maybe (fail "liftMaybe: Nothing") return

safeHead :: (Monad m, MonadFail m) => [a] -> m a
safeHead [] = fail "head of empty list"
safeHead (x : _) = return x

{-ppStrategy :: Show a => Strategy a -> String-}
{-ppStrategy = rec . toCore-}
{-where-}
{-rec s = -}
{-case s of-}
{-u :*: v  -> rec u ++ " + "  ++ rec v-}
{-u :|: v  -> rec u ++ " | "  ++ rec v-}
{-u :%: v  -> rec u ++ " || " ++ rec v-}
{-Atomic u -> "< " ++ rec u ++ " >"-}
{-Succeed  -> "1"-}
{-Fail     -> "0"-}
{-Rule r   -> showId r-}
{-Label _ (Rule r)   -> showId r-}
{-_        -> show s-}

-- prettyStrategy compact = rec . toCore
--   where
--    rec s =
--      case json of
--        Succeed  -> int 1
--        Fail     -> int 0
--        Rule r   -> text (showId r)
--        Label l (Rule r)   -> showId r
--          Number n  -> text (show n)
--          String s  -> str (escape s)
--          Boolean b -> text (if b then "true" else "false")
--          Null      -> text "null"
--          Array xs  -> make lbracket rbracket (map rec xs)
--          Object xs -> make lbrace rbrace (map (uncurry (<:>)) xs)

--    x <:> y | compact    = str x <> char ':' <> rec y
--            | isSimple y = str x <> string ": " <> rec y
--            | otherwise  = align (str x <> char ':' <> line <> indent 2 (rec y))

--    str = dquotes . text

--    make open close xs
--       | compact || length xs < 2 =
--            enclose open close (hcat (intersperse comma xs))
--       | otherwise =
--            align (vsep (zipWith (<+>) (open:repeat comma) xs ++ [close]))
--    isSimple (Array xs)  = null xs
--    isSimple (Object xs) = null xs
--    isSimple _           = True

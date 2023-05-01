-- -----------------------------------------------------------------------------
-- -- Copyright 2014, Open Universiteit Nederland. This file is distributed
-- -- under the terms of the GNU General Public License. For more information,
-- -- see the file "LICENSE.txt", which is included in the distribution.
-- -----------------------------------------------------------------------------

-- -----------------------------------------------------------------------------

-- -- |
-- -- Maintainer  :  alex@botkes.nl
-- -- Stability   :  provisional
-- -- Portability :  unknown
-- module Transformations.Test where

-- import Control.Arrow
-- import Control.Monad
-- import Data.List
-- import Data.Maybe
-- import Language.Haskell.Compile
-- import Language.Haskell.ExerciseConfig hiding (name)
-- import Language.Haskell.Property
-- import Language.Haskell.Syntax
-- import Language.Haskell.Test (SourceFile (..), h99)
-- import Language.Haskell.Transformations.Inline
-- import Language.Haskell.Transformations.Preprocess
-- import Language.Haskell.Utils (liftMaybe)
-- import System.FilePath

-- -- testTransformation :: (Module -> Module) -> IO ()
-- -- testTransformation transform = h99 >>= mapM_ f
-- --   where
-- --     f source = do
-- --         let m = compile $ content source
-- --         funName <- liftM function $ liftMaybe $ config source
-- --         let m' = transform m
-- --             inputTypes = typesOf funName
-- --         b <- equalityCheck funName inputTypes m m'
-- --         putStrLn $ name source ++ ':' : ' ' : show b

-- typesOf :: String -> [String]
-- typesOf n =
--     let err = error $ "Test.hs: no types for function " ++ n
--     in  fromMaybe err $
--             lookup
--                 n
--                 [ ("myreverse", ["[Int]"])
--                 , ("butlast", ["[Int]"])
--                 , ("compress", ["[Int]"])
--                 , ("dropevery", ["[Int]", "Int"])
--                 , ("dupli", ["[Int]"])
--                 , ("elementat", ["[Int]", "Int"])
--                 , ("mylast", ["[Int]"])
--                 , ("mylength", ["[Int]"])
--                 , ("pack", ["[Int]"])
--                 , ("palindrome", ["[Int]"])
--                 , ("removeat", ["Int", "[Int]"])
--                 , ("repli", ["[Int]", "Int"])
--                 , ("rotate", ["[Int]", "Int"])
--                 , ("slice", ["[Int]", "Int", "Int"])
--                 , ("split", ["[Int]", "Int"])
--                 ]

-- type TestPairs = [(String, String)]

-- testAllPreprocess :: IO ()
-- testAllPreprocess =
--     putStrLn $ "Inling: " ++ show (and testInline)

-- -- | Test inlining
-- testInline :: [Bool]
-- testInline =
--     testPairs
--         (let ns = [Ident "f"] in removeDeadCode ns . inline ns)
--         [ ("f = 1", "f = g where g = 1") -- where
--         , ("f = 1", "f = let g = 1 in g") -- let
--         , ("f = 2 + 3", "f = x + y where x = 2 ; y = z where z = 3") -- nested wheres
--         , ("f = 2", "f = let g = let h = 2 in h in g") -- nested lets
--         , ("f = 3", "f = g where g = let h = 3 in h") -- let in where
--         , ("f = 3", "f = let g = h where h = 3\n    in g") -- where in let
--         , ("f = 2 + 3", "f = g + h where h = 3\ng = 2") -- let in where
--         , ("f = 2 + 3", "f = g + h\nh = 3\ng = 2") -- top level decl
--         , ("f = 2 + 3", "f = g + h where h = 3\ng = 2") -- top level and where
--         , ("f = let h = 1 : h in h", "f = g\ng = h\n where h = 1 : h") -- top level decl with recursive where
--         , ("f = let h = 1 : h in h", "f = g\ng = let h = 1 : h in h") -- top level decl with recusive let
--         ]

-- testRewriteRecurPatBind :: [Bool]
-- testRewriteRecurPatBind =
--     testPairs
--         rewriteRecurPatBind
--         [ ("f = fix (\\ f -> \\ x -> x : f x)", "f = \\ x -> x : f x") -- normal fun binding
--         ]

-- testRewriteInvariantArgs :: [Bool]
-- testRewriteInvariantArgs =
--     testPairs
--         rewriteConstantArgs
--         [
--             ( "f = \\ c -> let f []     = c\n"
--                 ++ "               f (x:xs) = f xs + f xs\n"
--                 ++ "            in f"
--             , "f c []     = c\n"
--                 ++ "f c (x:xs) = f c xs + f c xs\n"
--             )
--         ]

-- -- | Help functions
-- testPairs :: (Module -> Module) -> TestPairs -> [Bool]
-- testPairs f = map (uncurry (==) . (compile *** f . compile))

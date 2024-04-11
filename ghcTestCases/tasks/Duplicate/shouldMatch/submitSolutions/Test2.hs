{-# OPTIONS_GHC -Wno-typed-holes #-}

module Test2 where

dupli :: [a] -> [a]
dupli [] = []
dupli (x : xs) = _ ++ dupli xs

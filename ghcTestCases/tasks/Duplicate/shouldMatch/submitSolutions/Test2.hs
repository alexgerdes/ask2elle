{-# OPTIONS_GHC -Wno-typed-holes #-}

module Test2 where


data Lsk = Alex | SiKai 

dupli :: [a] -> [a]
dupli [] = []
dupli (x : xs) = _ ++ dupli xs

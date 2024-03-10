{-# OPTIONS_GHC -Wno-typed-holes #-}
module Mod4 where

dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = [x,x] ++ dupli xs 

{- dupli (x:xs) = dup ++ dupli xs 
    where dup = [x,x] -}
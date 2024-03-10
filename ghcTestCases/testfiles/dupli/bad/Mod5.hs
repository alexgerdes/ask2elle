{-# OPTIONS_GHC -Wno-typed-holes #-}
module Mod5 where

dupli :: [a] -> [a]
dupli = foldr (\x xs -> x:x:xs) []
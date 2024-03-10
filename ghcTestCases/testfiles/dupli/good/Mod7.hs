{-# OPTIONS_GHC -Wno-typed-holes #-}
module Mod7 where


dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = [x,x] ++ dupli xs
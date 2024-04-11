{-# OPTIONS_GHC -Wno-typed-holes #-}

module Test3 where

dupli :: [a] -> [a]
dupli [] = []
dupli (x : xs) = dup ++ dupli xs
  where
    dup = [x, x]

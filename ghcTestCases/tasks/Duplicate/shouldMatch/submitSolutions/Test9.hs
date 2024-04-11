{-# OPTIONS_GHC -Wno-typed-holes #-}

module Test9 where

dupli :: [a] -> [a]
dupli xs = _ (replicate 2) xs

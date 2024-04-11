{-# OPTIONS_GHC -Wno-typed-holes #-}

module Test4 where

dupli :: [a] -> [a]
dupli = foldr _ _

{-# OPTIONS_GHC -Wno-typed-holes #-}

module Mod9 where

myreverse :: [a] -> [a]
myreverse = foldl (flip (:)) []

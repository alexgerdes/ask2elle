{-# OPTIONS_GHC -Wno-typed-holes #-}
module Mod4 where

myreverse :: [a] -> [a]
myreverse = foldl (flip (:)) []




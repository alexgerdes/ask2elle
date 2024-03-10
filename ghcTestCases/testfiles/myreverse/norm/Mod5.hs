{-# OPTIONS_GHC -Wno-typed-holes #-}
module Mod5 where

myreverse :: [a] -> [a]
myreverse = foldl (flip (:)) []

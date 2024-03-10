{-# OPTIONS_GHC -Wno-typed-holes #-}
module Mod1 where


f :: (Int -> Int -> Int) -> Int -> Int -> Int
f g = g

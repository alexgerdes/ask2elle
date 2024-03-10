{-# OPTIONS_GHC -Wno-typed-holes #-}
module Test1 where

f :: (Int -> Int -> Int) -> Int -> Int -> Int
f g x y = g x y
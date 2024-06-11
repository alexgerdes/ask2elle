{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}

module Test5 where

myreverse :: [a] -> [a]
myreverse (x : xs) = myreverse xs ++ [x]
myreverse [] = []
myreverse _ = []

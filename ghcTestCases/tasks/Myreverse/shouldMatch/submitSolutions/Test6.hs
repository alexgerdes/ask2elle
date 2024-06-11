{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}

module Test6 where

myreverse :: [a] -> [a]
myreverse (x : xs) = myreverse xs ++ [x]
myreverse [x] = [x]
myreverse [] = []

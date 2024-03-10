{-# OPTIONS_GHC -Wno-typed-holes #-}
module Test5 where

myreverse :: [a] -> [a]
myreverse xs = let f = flip (:) in let g = fold f in g [] xs 
    where fold = foldl


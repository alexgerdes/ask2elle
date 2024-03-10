{-# OPTIONS_GHC -Wno-typed-holes #-}
module Mod1 where 

myreverse :: [a] -> [a]
myreverse [] = []
myreverse (x:xs) = myreverse xs ++ [x]
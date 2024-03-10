{-# OPTIONS_GHC -Wno-typed-holes #-}
module Test1 where 

myreverse :: [a] -> [a]
myreverse [] = []
myreverse (x:xs) = _ ++ [x]
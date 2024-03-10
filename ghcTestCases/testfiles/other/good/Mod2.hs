module Mod2 where 

len :: [Int] -> Int 
len xs = case xs of 
    [] -> 0 
    ds -> length ds   
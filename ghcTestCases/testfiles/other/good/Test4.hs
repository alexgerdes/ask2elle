module Test4 where 


f :: [Int] -> Int
f xs = case xs of 
    []  -> 1
    [2] -> 2
    [3] -> 3
    _   -> 4 
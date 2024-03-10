module Test3 where 


f :: [Int] -> Int
f ds | ds == []  = 1
     | ds == [2] = 2
     | ds == [3] = 3
     | ds == ds  = 4


{- f :: [Int] -> Int 
f ds = case ds of 
     []  -> 1 
     [2] -> 2
     [3] -> 3 
     _   -> 4  -}
module Test1 where 

length' :: Num t => [a] -> t 
length' = len
    where len [] = 0
          len (x:xs) = 1 + len xs 

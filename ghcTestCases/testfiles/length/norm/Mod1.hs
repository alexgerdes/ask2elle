module Mod1 where 

length' :: Num t => [a] -> t 
length' [] = 0 
length' (x:xs) = 1 + length' xs 

{- length' :: Num t => [a] -> t
length' = len 
    where len [] = 0 
          len (x:xs) = 1 + len xs  -}
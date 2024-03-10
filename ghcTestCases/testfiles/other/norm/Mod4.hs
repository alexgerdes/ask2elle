module Mod4 where
{- 
length1 :: [a] -> Int
length1 = foldl (\x _ -> x + 1) 0 
 -}

length2 :: [a] -> Int 
length2 = foldl fun 0 
    where fun x _ = x + 1
          bun x = x + 3  
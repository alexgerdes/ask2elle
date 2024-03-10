{-# OPTIONS_GHC -Wno-typed-holes #-}
module Test3 where 

myreverse :: [a] -> [a]
myreverse = reverse' []
  
--reverse' :: [a] -> [a] -> [a]  -- type signature for inner function
reverse' acc [] = acc
reverse' acc (x:xs) = reverse' (x:acc) xs
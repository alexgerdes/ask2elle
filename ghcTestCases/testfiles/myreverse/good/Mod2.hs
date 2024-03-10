{-# OPTIONS_GHC -Wno-typed-holes #-}
module Mod2 where 

myreverse :: [a] -> [a]
myreverse = reverse' []
  where
    reverse' acc [] = acc
    reverse' acc (x:xs) = reverse' (x:acc) xs
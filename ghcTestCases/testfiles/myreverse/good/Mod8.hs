module Mod8 where 


myreverse :: [a] -> [a]
myreverse = reverse' []
  where
    reverse' acc [] = acc
    reverse' acc (x:xs) = reverse' (x:acc) xs
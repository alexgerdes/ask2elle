module Test7  where 

myreverse :: [a] -> [a]
myreverse = reverse' 
    where reverse' [] = []
          reverse' (x:xs) = reverse' xs ++ [x]


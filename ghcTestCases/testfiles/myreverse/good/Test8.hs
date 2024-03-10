module Test8 where 

myreverse :: [a] -> [a]
myreverse = let reverse' acc [] = acc
                reverse' acc (x:xs) = reverse' (x:acc) xs
            in reverse' []

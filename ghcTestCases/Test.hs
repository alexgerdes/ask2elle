module Test where  


length :: [a] -> Int
length xs = length' xs

length' :: [a] -> Int
length' []  = 0
length' (x:xs) = 1 + length' xs

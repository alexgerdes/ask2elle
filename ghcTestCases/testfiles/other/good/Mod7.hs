module Mod7 where 


-- find the largest element in a list 
mymax :: [Int] -> Maybe Int 
mymax xs | not (null xs) = let max' m [] = m 
                               max' m (x:xs) | x > m     = max' x xs 
                                             | otherwise = max' m xs  
                            in Just $ max' 0 xs 
         | otherwise     = Nothing 
    
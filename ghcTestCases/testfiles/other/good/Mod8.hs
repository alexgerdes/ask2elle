module Mod8 where 

mymax :: [Int] -> Maybe Int 
mymax xs = case xs of 
        []     -> Nothing 
        _      -> let max' m [] = m 
                      max' m (x:xs) | x > m     = max' x xs 
                                    | otherwise = max' m xs
                  in Just $ max' 0 xs  

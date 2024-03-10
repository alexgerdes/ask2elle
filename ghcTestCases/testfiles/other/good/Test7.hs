module Test7 where 


-- find the largest element in a list 
mymax :: [Int] -> Maybe Int 
mymax xs | not (null xs) = Just $ max' 0 xs 
         | otherwise     = Nothing 
    where max' m [] = m 
          max' m (x:xs) | x > m     = max' x xs 
                        | otherwise = max' m xs  
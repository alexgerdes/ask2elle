module Test8 where 

mymax :: [Int] -> Maybe Int 
mymax [] = Nothing 
mymax xs = Just $ max' 0 xs  
    where max' m [] = m 
          max' m (x:xs) = if x > m then max' x xs 
                                   else max' m xs  
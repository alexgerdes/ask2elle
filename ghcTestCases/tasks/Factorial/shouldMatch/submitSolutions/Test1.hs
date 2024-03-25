{-# OPTIONS_GHC -Wno-typed-holes #-}
module Test1 where 


factorial :: (Eq t, Num t) => t -> t
factorial m | m == 0    = 1 
            | otherwise = m * factorial (m - 1)  



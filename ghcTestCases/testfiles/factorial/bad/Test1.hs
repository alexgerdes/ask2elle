{-# OPTIONS_GHC -Wno-typed-holes #-}
module Test1 where 


factorial :: Int -> Int 
factorial 0 = 1 
factorial m = m * factorial (1-m)   
{-# OPTIONS_GHC -Wno-typed-holes #-}


module Test1 where 



factorial :: Integer -> Integer 
factorial = f   

f :: Integer -> Integer 
f 0 = 1 
f m = m * f (m - 1)  




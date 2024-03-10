{-# OPTIONS_GHC -Wno-typed-holes #-}
module Mod5 where 


factorial :: Integer -> Integer 
factorial = f 
    where f 0 = 1 
          f m = m * f (m - 1)   



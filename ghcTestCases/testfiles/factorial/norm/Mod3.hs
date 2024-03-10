{-# OPTIONS_GHC -Wno-typed-holes #-}
module Mod3 where 


factorial :: (Eq t, Num t) => t -> t 
factorial = f  
    where f 0 = 1 
          f m = m * f (m - 1)
{-# OPTIONS_GHC -Wno-typed-holes #-}
module Mod2 where 


factorial :: (Eq t, Num t) => t -> t
factorial 0 = 1 
factorial m = m * factorial (m - 1)    




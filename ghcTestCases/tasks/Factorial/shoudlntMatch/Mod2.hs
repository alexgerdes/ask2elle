{-# OPTIONS_GHC -Wno-typed-holes #-}
module Mod2 where 


factorial 0 = 1 
factorial m = m * factorial (m - 1)   -- non terminating recursion
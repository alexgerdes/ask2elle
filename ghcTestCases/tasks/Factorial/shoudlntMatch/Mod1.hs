{-# OPTIONS_GHC -Wno-typed-holes #-}
module Mod1 where 


factorial :: Int -> Int 
factorial 0 = 1 
factorial m = m * factorial (m - 1)   
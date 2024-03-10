{-# OPTIONS_GHC -Wno-typed-holes #-}
module Mod4 where 
    
fromBin :: Num a => [a] -> a
fromBin = sum . zipWith (*) (iterate (*2) 1) . reverse

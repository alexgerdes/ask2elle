{-# OPTIONS_GHC -Wno-typed-holes #-}
module Test3 where 
    
fromBin :: Num a => [a] -> a
fromBin bits = foldl _ 0 bits

{-
fromBin :: Num a => [a] -> a
fromBin bits = foldl (\acc bit -> acc * 2 + bit) 0 bits
-}
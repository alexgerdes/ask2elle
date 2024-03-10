{-# OPTIONS_GHC -Wno-typed-holes #-}
module Test1 where 

fromBin :: Num a => [a] -> a
fromBin bs = let n = length bs in fromBin' (n - 1) bs
  where
    fromBin' _ []    = 0
    fromBin' n (b:bs) = 2^n * b + fromBin' (n-1) bs
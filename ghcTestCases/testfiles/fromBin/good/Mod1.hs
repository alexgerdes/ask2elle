{-# OPTIONS_GHC -Wno-typed-holes #-}
module Mod1 where 

fromBin :: Num a => [a] -> a
fromBin bs = fromBin' (length bs - 1) bs
  where
    fromBin' n []     = 0
    fromBin' n (b:bs) = 2^n * b + fromBin' (n-1) bs
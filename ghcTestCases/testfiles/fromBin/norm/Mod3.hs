{-# OPTIONS_GHC -Wno-typed-holes #-}
module Mod3 where 

fromBin bs = fromBin' (length bs - 1) bs
  where
    fromBin' _ []     = 0
    fromBin' n (b:bs) = 2^n * b + fromBin' (n-1) bs
{-# OPTIONS_GHC -Wno-typed-holes #-}
module Test3 where 

fromBin bs = fromBin' l bs
  where l = length bs - 1      -- defined inline in model solution
        fromBin' n []     = 0
        fromBin' n (b:bs) = 2^n * b + fromBin'(n-1) bs
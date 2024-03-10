{-# OPTIONS_GHC -Wno-typed-holes #-}
module Test1 where 


fromBin :: Num a => [a] -> a
fromBin bs = fromBin' (length bs-1) bs
  where fromBin' n []     = 0
        fromBin' n (b:bs) = _ + fromBin' (n-1) bs
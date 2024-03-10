{-# OPTIONS_GHC -Wno-typed-holes #-}
module Test1 where 

fromBin :: Num a => [a] -> a
fromBin bs = fromBin' bs
  where
    fromBin' []     = 0
    fromBin' (b:bs) = 2^(length bs - 1) * b + fromBin' bs
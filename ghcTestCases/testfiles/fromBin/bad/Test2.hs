{-# OPTIONS_GHC -Wno-typed-holes #-}
module Test2 where 

fromBin :: Num a => [a] -> a
fromBin = foldl op 1
    where 
      n `op` b = 2*n + b
{-# OPTIONS_GHC -Wno-typed-holes #-}
module Test2 where 

fromBin :: Num a => [a] -> a
fromBin = foldl op 0
    where 
      op n b = _ 

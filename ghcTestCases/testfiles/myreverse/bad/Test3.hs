{-# OPTIONS_GHC -Wno-typed-holes #-}
module Test3 where 


myreverse :: [a] -> [a]
myreverse xs = reverse' xs []
  where
    reverse' acc [] = acc
    reverse' acc (x:xs) = reverse' (x:acc) xs
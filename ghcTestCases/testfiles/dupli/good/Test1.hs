--{-# OPTIONS_GHC -Wno-typed-holes #-}
module Test1 where 

dupli :: [a] -> [a] 
dupli xs = concatMap (replicate 2) (_ :: [a]) 



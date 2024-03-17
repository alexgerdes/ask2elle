--{-# OPTIONS_GHC -Wno-typed-holes #-}
module Duplicate where 

dupli :: [a] -> [a] 
dupli xs = concatMap (replicate 2) (_ :: [a]) 



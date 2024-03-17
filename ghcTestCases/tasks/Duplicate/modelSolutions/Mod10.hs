{-# OPTIONS_GHC -Wno-typed-holes #-}
module Mod10 where 


dupli :: [a] -> [a]
dupli = concatMap (replicate 2) 


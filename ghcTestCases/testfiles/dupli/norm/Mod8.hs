{-# OPTIONS_GHC -Wno-typed-holes #-}
module Mod8 where 


dupli :: [a] -> [a]
dupli = concatMap (replicate 2) 


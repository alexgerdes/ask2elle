{-# OPTIONS_GHC -Wno-typed-holes #-}
module Mod11 where
    
dupli :: [a] -> [a]
dupli = concatMap (\x -> [x,x])

{-# OPTIONS_GHC -Wno-typed-holes #-}
module Mod7 where
    
dupli :: [a] -> [a]
dupli = concatMap (\x -> [x,x])

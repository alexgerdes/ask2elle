{-# OPTIONS_GHC -Wno-typed-holes #-}
module Mod12 where 


dupli :: [a] -> [a]
dupli xs = concatMap (\x -> [x,x])  xs 


module Mod8 where 


{-# RULES
    "map/listcomprh"   forall f xs.  map f xs = [f x | x <- xs] 
#-}



-- this rule does not fire, it might not be considered "cheap" enough by ghc.

dupli :: [a] -> [a]
dupli xs = concat (map (\x -> [x,x]) xs)


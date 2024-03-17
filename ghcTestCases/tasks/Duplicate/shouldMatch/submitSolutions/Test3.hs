{-# OPTIONS_GHC -Wno-typed-holes #-}
module Test4 where 


dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = dup ++ dupli xs
    where dup = [x,x]


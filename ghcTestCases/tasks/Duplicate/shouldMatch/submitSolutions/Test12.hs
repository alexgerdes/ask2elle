{-# OPTIONS_GHC -Wno-typed-holes #-}
module Test12 where 


dupli :: [a] -> [a]
dupli = concatMap _ 


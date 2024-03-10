{-# OPTIONS_GHC -Wno-typed-holes #-}
module Test5 where 


dupli :: [a] -> [a]
dupli = foldr _ _  




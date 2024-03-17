{-# OPTIONS_GHC -Wno-typed-holes #-}
module Test7 where 

dupli :: [a] -> [a]
dupli xs = concatMap (\x -> [x,x]) xs  


{-# OPTIONS_GHC -Wno-typed-holes #-}

module Test6 where 

dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = [x,x] ++ dupli _H


{-# OPTIONS_GHC -Wno-typed-holes #-}

module Test6 where


dupli :: [a] -> [a]
dupli xs = case xs of  
    [] -> []
    (x:xs) -> [x,x] ++ dupli xs
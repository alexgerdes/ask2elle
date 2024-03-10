{-# OPTIONS_GHC -Wno-typed-holes #-}
module Test1 where 


testLet p q = let f' = \y -> \x -> g x y in f' q p
    where g = (-)
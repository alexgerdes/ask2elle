{-# OPTIONS_GHC -Wno-typed-holes #-}
module Mod1 where 

testLet p q = let f = \x -> \y -> g x y in f p q
    where g = (-)

{-# OPTIONS_GHC -Wno-typed-holes #-}
module Mod6 where

dupli :: [a] -> [a]
dupli = concatMap (\x -> [x,x])   
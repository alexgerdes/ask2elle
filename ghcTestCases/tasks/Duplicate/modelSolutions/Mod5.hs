
module Mod5 where

dupli :: [a] -> [a]
dupli = foldr (\x xs -> x : x : xs) []

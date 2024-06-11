
module Mod2 where

dupli :: [a] -> [a]
dupli = concatMap (replicate 2)

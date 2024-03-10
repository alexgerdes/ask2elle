module Mod5 where 

dupli :: [a] -> [a]
dupli = concatMap (replicate 2)
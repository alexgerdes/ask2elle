module Mod9 where 

dupli :: [a] -> [a]
dupli = concatMap (replicate 2)
{-# OPTIONS_GHC -Wno-typed-holes #-}
--{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Test1 where 

myreverse :: [a] -> [a]
myreverse _      = []
myreverse (x:xs) = x:xs 
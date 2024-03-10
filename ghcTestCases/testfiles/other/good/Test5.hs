{-# OPTIONS_GHC -Wno-typed-holes #-}
module Test5 where 

duprevzip :: [Int] -> [Int] -> [(Int,Int)]
duprevzip xs ys = let rev [] = []
                      rev (x:xs) = rev xs ++ [x]
                      dup [] = []
                      dup (x:xs) = x:x:dup xs  
                  in zip (dup xs) (rev ys)
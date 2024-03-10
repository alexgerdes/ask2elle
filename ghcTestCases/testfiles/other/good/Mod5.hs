module Mod5 where 

duprevzip :: [Int] -> [Int] -> [(Int,Int)]
duprevzip xs ys = let dup [] = []
                      dup (x:xs) = x:x:dup xs 
                      rev [] = []
                      rev (x:xs) = rev xs ++ [x]
                  in zip (dup xs) (rev ys)
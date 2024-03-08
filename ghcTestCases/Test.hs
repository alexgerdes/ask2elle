module Test where  

import Data.Maybe 

test :: [Int] -> [Int]
test xs =  map (+1) $ (map (+2) xs)
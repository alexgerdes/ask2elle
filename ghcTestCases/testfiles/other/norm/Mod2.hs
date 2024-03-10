module Mod2 where 

{-
{-# RULES
    "pointfree/lambda" forall a. forall (f :: a -> a) (g :: a -> a) (xs :: [a]). map (f . g) xs = map (\z -> f (g z)) xs 
#-}
-}

-- test rewrite rules pragmas 
addAndMult :: Int -> [Int] -> [Int]
addAndMult n xs = map ((*n) . (+n)) xs 
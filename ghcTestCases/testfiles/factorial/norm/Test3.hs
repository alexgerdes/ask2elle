{-# OPTIONS_GHC -Wno-typed-holes #-}
module Test3 where 


-- expects match for all these definitions

factorial :: (Eq t, Num t) => t -> t 
factorial = f   

f 0 = 1 
f m = m * f (m - 1)  
 


{- factorial :: Integer -> Integer 
factorial n = f n  
    where f :: Integer -> Integer 
          f 0 = 1 
          f m = m * f (m - 1)  -}


-- does not match this due to different inferred types
-- the inferred type of f, even when feeding n to f
-- is f :: (Eq t, Num t) => t -> t which
-- creates an additional application off fromInteger 
{- factorial :: Integer -> Integer 
factorial n = f n 
    where 
          f 0 = 1 
          f m = m * f (m - 1)   -}







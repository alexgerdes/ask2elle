{-# OPTIONS_GHC -Wno-typed-holes #-}
module Mod1 where 


factorial :: Integer -> Integer 
factorial 0 = 1 
factorial m = m * factorial (m - 1)   

-- currently rewritten to 
-- factorial = \eta -> let f = \ds -> case ds == 0 of 
--                      False -> ds * f (ds - 1)
--                      True  -> 1 
--             in f eta 

-- which we want, 
-- but in this definition, every occurence of ds is constraint to be Eq and Num 
-- which we don't have if defining factorial like following
-- the eq constraint on ds shows up only in the case (where we do the equality check)
-- and then the Num constraint shows up only inside the case alternatives
{-
factorial :: (Eq t, Num t) => t -> t 
factorial = f 
    where f 0 = 1 
          f m = m * f (m-1)
-}
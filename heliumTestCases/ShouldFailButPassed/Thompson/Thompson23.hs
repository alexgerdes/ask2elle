-- ! Helium fails to recognize the Eq a constraint from (==) 
empty :: [a] -> Bool 
empty as = (as == []) 
{- helium does not have a class Eq -}

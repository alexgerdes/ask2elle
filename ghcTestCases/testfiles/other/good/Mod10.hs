module Mod10 where 


palindrome :: Eq a => [a] -> Bool
palindrome xs = xs == reverse xs 
 

{- p :: Eq t => t -> t -> Bool  
p = (/=)

b :: Eq t => t -> t -> Bool  
b = (==) -}
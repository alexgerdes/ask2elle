module Mod6 where 


palindrome :: Eq a => [a] -> Bool 
palindrome xs = xs == (reverse xs)
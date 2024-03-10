--{-# OPTIONS_GHC -fplugin=Splint  #-}
module Test6 where 


palindrome :: Eq a => [a] -> Bool 
palindrome xs | xs == reverse xs = True 
              | otherwise        = False  

{- palindrome :: Eq a => [a] -> Bool
palindrome [] = True
palindrome xs = if zs == xs then True else False
    where zs = reverse xs -}
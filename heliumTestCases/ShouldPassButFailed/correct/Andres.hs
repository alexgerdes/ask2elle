{-
HeliumTypeCheckerError
(6,37): Type error in variable
 expression       : +
   type           : Int
   expected type  : a -> a -> a
-}
module Andres where

test1 = let f (+) = 3 + 5 in f (-)

test2 = let ((+) : xs) = [2, 3] in (+)

test3 = let f x = 5; y = 4 in -f y

test4 = do
  let f = return ()
   in f

main = 1

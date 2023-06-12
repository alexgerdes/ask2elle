-- ! looks good to me, maybe a runtime error, but in the realm of frontend
module Daan where

main :: Int
main = let f x = f (const x 1) in f 0
   
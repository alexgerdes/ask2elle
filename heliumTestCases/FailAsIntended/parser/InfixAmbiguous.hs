module InfixAmbiguous where

infix 9 +++

main = 3 +++ 4 +++ 4

(+++) x y = x

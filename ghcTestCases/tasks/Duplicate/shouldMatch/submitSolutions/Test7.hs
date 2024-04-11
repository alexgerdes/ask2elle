module Test7 where

dupli :: [a] -> [a]
dupli xs = concat [[x, x] | x <- xs]

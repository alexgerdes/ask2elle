module Test6 where 


factorial :: (Eq t, Num t) => t -> t
factorial 0 = 1 
factorial m = factorial (m - 1) * m --- test commutativity
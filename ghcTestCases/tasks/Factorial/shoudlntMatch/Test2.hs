module Test2 where 


factorial 0 = 1 
factorial m = factorial m * factorial (m - 1)   -- non terminating recursion
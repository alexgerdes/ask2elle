module Test5 where 


factorial :: Integer -> Integer 
factorial n = f n 
    where f 0 = 1 
          f m = m * f (m - 1)  
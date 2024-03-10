module Test11 where 
    
{-# RULES "map/map" forall f g xs.  map f (map g xs) = map (f . g) xs #-}

-- test map fusion

f :: Num b => [b] -> [b]
f xs = map (*2) (map (+2) xs)
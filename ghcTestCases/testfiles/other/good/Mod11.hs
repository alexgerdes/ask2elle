module Mod11 where


{-# RULES
    "map/map"    forall f g xs.  map f (map g xs) = map (f . g) xs
#-}
-- test map fusion

f :: Num b => [b] -> [b]
f = map ((*2) . (+2)) 

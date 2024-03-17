module Test5 where 


{-# RULES "concatMap/map" forall f xs. concat (map f xs) = concatMap f xs  #-}
{-# RULES "concat.map/concatMap" forall f.  concat . map f = concatMap f #-}


{- dupli :: [a] -> [a]
dupli xs = concat (map (replicate 2) xs) -}

dupli :: [a] -> [a]
dupli = concat . map (replicate 2)
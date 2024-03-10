module Test1 where 


sub :: Int -> Int -> Int 
sub x y = (-) y x 


--[NonRec sub (Lam x (Lam y (App (App (App (App (Var -) (Type (TyConApp Int []))) (Var $fNumInt)) (Var y)) (Var x))))]
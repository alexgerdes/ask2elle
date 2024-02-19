-- inline top level binding , which is only exactly once, into call site 
```haskell
length :: [a] -> Int
length xs = length' xs

length' :: [a] -> Int
length' []  = 0
length' (x:xs) = 1 + length' xs


[NonRec $trModule (App (App (Var Module) (App (Var TrNameS) (Lit main))) (App (Var TrNameS) (Lit Test))),

NonRec length 
  (Lam a_axn  -- * List element type 
    (Let 
      (Rec 
        [(length'_rfZ,
          Lam a_awu 
            (Lam ds_dxJ 
              (Tick src<ghcTestCases/Test.hs:(7,1)-(8,31)> 
                (Case 
                  (Var ds_dxJ) 
                  wild_00 
                  (TyConApp Int []) 
                  [Alt 
                   (DataAlt []) 
                   [] 
                   (Tick src<ghcTestCases/Test.hs:7:15> (App (Var I#) (Lit 0))),
                   Alt 
                    (DataAlt :) 
                    [x_atQ,xs_atR] 
                    (Tick src<ghcTestCases/Test.hs:8:18-31> 
                      (App 
                        (App 
                          (App 
                            (App (Var +) (Type (TyConApp Int []))) 
                            (Var $fNumInt)) 
                          (Tick src<ghcTestCases/Test.hs:8:18> (App (Var I#) (Lit 1)))) 
                        (Tick src<ghcTestCases/Test.hs:8:22-31> 
                          (App 
                            (App (Var length'_rfZ) (Type (TyVarTy a_awu))) 
                            (Tick src<ghcTestCases/Test.hs:8:30-31> (Var xs_atR))))))]))))]) 
      (Lam xs_atP   -- * input for length
        (Tick src<ghcTestCases/Test.hs:5:1-22> 
          (App       -- * apply length' with input
            (App 
              (Var length'_rfZ) (Type (TyVarTy a_axn))) 
            (Tick src<ghcTestCases/Test.hs:5:21-22> (Var xs_atP)))))))]
```
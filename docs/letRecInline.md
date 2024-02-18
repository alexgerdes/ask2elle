```haskell
length :: [a] -> Int 
length [] = 0 
length (x:xs) = 1 + length xs

-- the top level recursive fn compiles to non-rec binder
[NonRec length' 
  (Let 
    (Rec [
        (length'_Rxw, 
          Lam a_awg 
            (Lam ds_dxs (Tick src<ghcTestCases/Test.hs:(5,1)-(6,31)> 
            (Case 
              (Var ds_dxs) 
              wild_00 
              (TyConApp Int []) 
              [Alt 
                  (DataAlt []) 
                  [] 
                  (Tick src<ghcTestCases/Test.hs:5:13> 
                  (App (Var I#) (Lit 0))),
               Alt 
                  (DataAlt :) 
                  [x_atN,xs_atO] 
                  (Tick src<ghcTestCases/Test.hs:6:18-31> 
                    (App 
                      (App 
                        (App 
                          (App (Var +) (Type (TyConApp Int []))) 
                          (Var $fNumInt)) 
                        (Tick src<ghcTestCases/Test.hs:6:18> 
                          (App 
                            (Var I#) (Lit 1))))
                      (Tick src<ghcTestCases/Test.hs:6:22-31> 
                        (App 
                          (App 
                            (Var length'_Rxw) (Type (TyVarTy a_awg))) 
                          (Tick src<ghcTestCases/Test.hs:6:30-31> (Var xs_atO))))))]))))]) 
      (Var length'_Rxw)),
      NonRec $trModule (App (App (Var Module) (App (Var TrNameS) (Lit main))) (App (Var TrNameS) (Lit Test)))]
```
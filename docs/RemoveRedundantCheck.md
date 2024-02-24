```haskell

f x y | x == y = False
      | otherwise = True


[
  NonRec f 
    (Lam a_aFD 
      (Lam $dEq_aHM 
        (Lam x_ag0 
          (Lam y_ag1 (Tick src<ghcTestCases/Test.hs:(4,1)-(5,24)> 
            (Case (Tick src<ghcTestCases/Test.hs:4:9-14> 
              (App 
                (App 
                  (App 
                    (App (Var ==) (Type (TyVarTy a_aFD))) 
                    (Var $dEq_aHM)) 
                  (Tick src<ghcTestCases/Test.hs:4:9> (Var x_ag0))) 
                (Tick src<ghcTestCases/Test.hs:4:14> (Var y_ag1)))) 
                wild_00 
                (TyConApp Bool []) 
                [Alt (DataAlt False) [] (Tick src<ghcTestCases/Test.hs:5:9-17> (Tick src<ghcTestCases/Test.hs:5:21-24> (Var True))),
                 Alt (DataAlt True) [] (Tick src<ghcTestCases/Test.hs:4:18-22> (Var False))])))))),
                 
  NonRec $trModule (App (App (Var Module) (App (Var TrNameS) (Lit main))) (App (Var TrNameS) (Lit Test)))]
```
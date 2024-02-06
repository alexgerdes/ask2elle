```haskell

dupli :: [Int] ->  [Int]
dupli [] = []
dupli [x] = [x,x]
dupli (x:xs) = [x,x] ++ dupli xs

-- * After Desugared Core
[Rec [
  (dupli,
    Lam ds_dul (Tick src<ghcTestCases/Test.hs:(4,1)-(6,32)> 
      (Case 
        (Var ds_dul) 
        wild_00 
        (TyConApp [] [TyConApp Int []]) 
        [Alt (DataAlt []) [] (App (Tick src<ghcTestCases/Test.hs:4:12-13> (Var [])) (Type (TyConApp Int []))), -- * Empty List Case
         Alt (DataAlt :) [x_atM,ds_dur]                                                                        -- * (x:xs)
          (Case 
            (Var ds_dur) 
              wild_X1 
              (TyConApp [] [TyConApp Int []]) 
              [Alt DEFAULT [] (Tick src<ghcTestCases/Test.hs:6:16-32> 
                (App 
                  (App 
                    (App (Var ++) (Type (TyConApp Int [])))                                                     -- *  (++)
                    (Tick src<ghcTestCases/Test.hs:6:16-20>                                                     -- * LHS : [x,x] desugars to (x: (x: []))
                      (App 
                        (App 
                          (App (Var :) (Type (TyConApp Int []))) 
                          (Tick src<ghcTestCases/Test.hs:6:17> (Var x_atM))) 
                        (App 
                          (App 
                            (App (Var :) (Type (TyConApp Int []))) 
                            (Tick src<ghcTestCases/Test.hs:6:19> (Var x_atM))) 
                          (App (Var []) (Type (TyConApp Int []))))))) 
                    (Tick src<ghcTestCases/Test.hs:6:25-32>                                                    -- * RHS of (++)
                      (App 
                        (Var dupli) 
                        (Tick src<ghcTestCases/Test.hs:6:31-32> (Var ds_dur)))))),
                 Alt (DataAlt []) [] (Tick src<ghcTestCases/Test.hs:5:13-17>                                  -- * Singleton list desugars to (x: x : [])
                  (App 
                    (App 
                      (App (Var :) (Type (TyConApp Int []))) 
                      (Tick src<ghcTestCases/Test.hs:5:14> (Var x_atM))) 
                    (App 
                      (App 
                        (App (Var :) (Type (TyConApp Int []))) 
                        (Tick src<ghcTestCases/Test.hs:5:16> (Var x_atM))) 
                      (App (Var []) (Type (TyConApp Int []))))))])])))]
                      
,NonRec $trModule (App (App (Var Module) (App (Var TrNameS) (Lit main))) (App (Var TrNameS) (Lit Test)))]
```
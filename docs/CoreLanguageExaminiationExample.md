```haskell
dropevery [] _ = []
dropevery list count = (take (count-1) list) ++ dropevery (drop count list) count

-- -> 
[NonRec dropevery (Lam a_azo ( -- * element type
  Let (Rec [
    (dropevery_akm,
      Lam ds_dzO ( -- * list 
        Lam ds_dzP  -- * count 
          (Tick src<ghcTestCases/Test.hs:(5,1)-(6,81)>
           (Case (Var ds_dzO) wild_00 (TyConApp [] [TyVarTy a_azo]) -- 
            [Alt DEFAULT []
              (Tick src<ghcTestCases/Test.hs:6:24-81>
               (App 
                  (App (App 
                            (Var ++) 
                            (Type (TyVarTy a_azo)))   -- * type application : (++) @a_azo 
                    (Tick src<ghcTestCases/Test.hs:6:24-44> 
                      (App 
                        (App 
                          (App (Var take) (Type (TyVarTy a_azo))) -- * type application : take @a_azo, take :: \forall a. Int -> [a] -> [a]
                          (Tick src<ghcTestCases/Test.hs:6:30-38> 
                            (App (App 
                                    (App 
                                      (App (Var -) (Type (TyConApp Int []))) -- * (-) desugared to : \@Int \$fNumDict \fstInput \sndInput
                                      (Var $fNumInt))                      
                                    (Tick src<ghcTestCases/Test.hs:6:31-35> (Var ds_dzP))) 
                                 (Tick src<ghcTestCases/Test.hs:6:37> (App (Var I#) (Lit 1)))))) 
                        (Tick src<ghcTestCases/Test.hs:6:40-43> (Var ds_dzO))))) 
                  (Tick src<ghcTestCases/Test.hs:6:49-81> 
                    (App (App (Var dropevery_akm) 
                              (Tick src<ghcTestCases/Test.hs:6:59-75> 
                                (App (App (App (Var drop) (Type (TyVarTy a_azo))) 
                                          (Tick src<ghcTestCases/Test.hs:6:65-69> (Var ds_dzP))) 
                                     (Tick src<ghcTestCases/Test.hs:6:71-74> (Var ds_dzO))))) 
                          (Tick src<ghcTestCases/Test.hs:6:77-81> (Var ds_dzP))))))
            ,Alt (DataAlt []) [] 
              (App (Tick src<ghcTestCases/Test.hs:5:18-19> (Var [])) (Type (TyVarTy a_azo))) -- * type application : [] @a_azo
            ]
          )
         )
        )
      )
    ]
  ) (Var dropevery_akm)))
  ,NonRec $trModule (App (App (Var Module) (App (Var TrNameS) (Lit main))) (App (Var TrNameS) (Lit Test)))]
```
```haskell
module Test where  


even' :: Int -> Bool
even' 0 = True
even' n = odd' (n - 1)

odd' :: Int -> Bool
odd' 0 = False
odd' n = even' (n - 1)
```
The following is a showcase for which scenario for `Rec [(b, Expr b)]`, also an example of more than 2 values in `Rec`
```haskell
[ -- *
  Rec [
  (even',
    Lam ds_dJt 
      (Tick src<ghcTestCases/Test.hs:(5,1)-(6,22)> 
        (Case 
          (Var ds_dJt) 
          wild_00 
          (TyConApp Bool []) 
          [Alt 
            (DataAlt I#) 
            [ds_dJu] 
            (Case 
              (Var ds_dJu) 
              ds_X1 
              (TyConApp Bool []) 
              [Alt 
                DEFAULT 
                [] 
                (Tick src<ghcTestCases/Test.hs:6:11-22> 
                  (App 
                    (Var odd') 
                    (Tick src<ghcTestCases/Test.hs:6:16-22> 
                      (App 
                        (App 
                          (App 
                            (App (Var -) (Type (TyConApp Int []))) 
                            (Var $fNumInt)) 
                          (Tick src<ghcTestCases/Test.hs:6:17> (Var ds_dJt))) 
                        (Tick src<ghcTestCases/Test.hs:6:21> (App (Var I#) (Lit 1))))))),
                 Alt 
                  (LitAlt 0) 
                  [] 
                  (Tick src<ghcTestCases/Test.hs:5:11-14> (Var True))])]))),
    (odd',
      Lam ds_dJn 
        (Tick src<ghcTestCases/Test.hs:(9,1)-(10,22)> 
        (Case 
          (Var ds_dJn) 
          wild_00 
          (TyConApp Bool []) 
          [Alt (DataAlt I#) [ds_dJo] 
            (Case 
              (Var ds_dJo) 
              ds_X1 
              (TyConApp Bool []) 
              [Alt 
                DEFAULT 
                [] 
                (Tick src<ghcTestCases/Test.hs:10:10-22> 
                  (App 
                    (Var even')
                    (Tick src<ghcTestCases/Test.hs:10:16-22> 
                      (App 
                        (App 
                          (App 
                            (App (Var -) (Type (TyConApp Int []))) 
                            (Var $fNumInt)) 
                          (Tick src<ghcTestCases/Test.hs:10:17> (Var ds_dJn))) 
                        (Tick src<ghcTestCases/Test.hs:10:21> 
                          (App (Var I#) (Lit 1))))))),
               Alt (LitAlt 0) [] (Tick src<ghcTestCases/Test.hs:9:10-14> (Var False))])])))],
               
        
               NonRec $trModule (App (App (Var Module) (App (Var TrNameS) (Lit main))) (App (Var TrNameS) (Lit Test)))]

```

by `inlineBinds` from `GhcLib.Transform.Inline`, we could inline 


```haskell
[NonRec $trModule (App (App (Var Module) (App (Var TrNameS) (Lit main))) (App (Var TrNameS) (Lit Test))),

Rec [
  (even',
    Lam ds_dJt (Tick src<ghcTestCases/Test.hs:(5,1)-(6,22)> 
      (Case 
        (Var ds_dJt) 
        wild_00 
        (TyConApp Bool []) 
        [Alt 
          (DataAlt I#) 
          [ds_dJu] 
          (Case 
            (Var ds_dJu) 
            ds_X1 
            (TyConApp Bool []) 
            [Alt 
              DEFAULT 
              [] 
              (Tick src<ghcTestCases/Test.hs:6:11-22> 
                (App 
                  (Var odd') 
                  (Tick src<ghcTestCases/Test.hs:6:16-22> 
                    (App 
                      (App 
                        (App 
                          (App (Var -) (Type (TyConApp Int []))) 
                          (Var $fNumInt)) 
                        (Tick src<ghcTestCases/Test.hs:6:17> (Var ds_dJt))) 
                      (Tick src<ghcTestCases/Test.hs:6:21> (App (Var I#) (Lit 1))))))),
               Alt 
                (LitAlt 0) 
                [] 
                (Tick src<ghcTestCases/Test.hs:5:11-14> (Var True))])]))),
    (odd',Lam ds_dJn (Tick src<ghcTestCases/Test.hs:(9,1)-(10,22)> (Case (Var ds_dJn) wild_00 (TyConApp Bool []) [Alt (DataAlt I#) [ds_dJo] (Case (Var ds_dJo) ds_X1 (TyConApp Bool []) [Alt DEFAULT [] (Tick src<ghcTestCases/Test.hs:10:10-22> (App (Var even') (Tick src<ghcTestCases/Test.hs:10:16-22> (App (App (App (App (Var -) (Type (TyConApp Int []))) (Var $fNumInt)) (Tick src<ghcTestCases/Test.hs:10:17> (Var ds_dJn))) (Tick src<ghcTestCases/Test.hs:10:21> (App (Var I#) (Lit 1))))))),Alt (LitAlt 0) [] (Tick src<ghcTestCases/Test.hs:9:10-14> (Var False))])])))]]


```
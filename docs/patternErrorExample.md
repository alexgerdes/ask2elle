Source code 
```haskell
module Test where  

import Data.Maybe 

test :: Maybe Int -> Maybe Int 
test x = case x of 
    Just y -> Just y

-- >
[NonRec test (Lam x_auF (Tick src<ghcTestCases/Test.hs:(6,1)-(7,20)> 
  (Case (Tick src<ghcTestCases/Test.hs:6:15> (Var x_auF))  -- * scrutinee
    wild_00                                                -- * varaible binds to that scrutinee
    (TyConApp Maybe [TyConApp Int []])                     -- * case return type
    [Alt DEFAULT [] 
      (Case 
        (App 
          (App 
            (App (Var patError) (Type (TyConApp LiftedRep [])))  -- * This line is the reason, why we need to use `universe` instead of `children`,
                                                                 -- * VarName is deeply nested in the expression
            (Type (TyConApp () []))) 
          (Lit ghcTestCases/Test.hs:(6,10)-(7,20)|case))         -- TODO : Just a thought, We could match aginst "|case" 
                                                                 --           to determine the patError
        wild_X1 
        (TyConApp Maybe [TyConApp Int []]) 
        []),
     Alt (DataAlt Just) [y_auG] 
      (Tick src<ghcTestCases/Test.hs:7:15-20> 
        (App 
          (App (Var Just) (Type (TyConApp Int []))) 
          (Tick src<ghcTestCases/Test.hs:7:20> (Var y_auG))))
    ]))),NonRec $trModule (App (App (Var Module) (App (Var TrNameS) (Lit main))) (App (Var TrNameS) (Lit Test)))]
```


```haskell

data Expr b	-- "b" for the type of binders, 
  = Var	  Var
  | Lit   Literal
  | App   (Expr b) (Arg b)
  | Lam   b (Expr b)
  | Let   (Bind b) (Expr b)
  | Case  (Expr b) b Type [Alt b]
  | Cast  (Expr b) Coercion
  | Tick  (Tickish Id) (Expr b)
  | Type  Type
  | Coercion Coercion

type Arg b = Expr b
type Alt b = (AltCon, [b], Expr b)

data AltCon = DataAlt DataCon | LitAlt  Literal | DEFAULT

data Bind b = NonRec b (Expr b) | Rec [(b, (Expr b))]

type CoreProgram = [CoreBind]
type CoreBndr = Var
type CoreExpr = Expr CoreBndr
type CoreBind = Bind CoreBndr

data Var
  = TyVar {  -- Type and kind variables
        varName    :: !Name,
        realUnique :: {-# UNPACK #-} !Int,
    }
  | TcTyVar { -- Used for type and kind variables inference                       
        varName        :: !Name,
        realUnique     :: {-# UNPACK #-} !Int,
        varType        :: Kind,
        tc_tv_details  :: TcTyVarDetails
  }
  | Id {
        varName    :: !Name,
        realUnique :: {-# UNPACK #-} !Int,
  }
```
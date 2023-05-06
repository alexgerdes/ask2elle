{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- UUAGC 0.9.53.1 (src/haskell-language/Language/Haskell/Generated/Syntax.ag)
module Language.Haskell.Generated.Syntax where

import Data.Data
import Data.Generics.Uniplate.Direct

type HoleID = String

rEFACTOR_FUNCTION_NAME, rEFACTOR_ARGS_NAME :: String
rEFACTOR_FUNCTION_NAME = "thefunction"
rEFACTOR_ARGS_NAME = "?theargs"

{-# LINE 243 "src/Language/Haskell/Syntax.hs" #-}
-- Alt ---------------------------------------------------------
data Alt
    = AHole (HoleID)
    | Alt ((Maybe String)) (Pat) (Rhs)
    | AltEmpty
    deriving (Data, Eq, Ord, Read, Show, Typeable)

-- Alts --------------------------------------------------------
type Alts = [Alt]

-- Body --------------------------------------------------------
data Body
    = BHole
    | Body (Decls)
    deriving (Data, Eq, Ord, Read, Show, Typeable)

-- Decl --------------------------------------------------------
data Decl
    = DHole (HoleID)
    | DEmpty
    | DFunBinds (FunBinds)
    | DPatBind (Pat) (Rhs)
    deriving (Data, Eq, Ord, Read, Show, Typeable)

-- Decls -------------------------------------------------------
type Decls = [Decl]

-- Expr --------------------------------------------------------
data Expr
    = Hole (HoleID)
    | Feedback (String) (Expr)
    | MustUse (Expr)
    | Eta (Int) (Expr)
    | Refactor (Expr) (RefactorChoices)
    | Case (Expr) (Alts)
    | Con (Name)
    | If (Expr) (Expr) (Expr)
    | InfixApp (MaybeExpr) (Expr) (MaybeExpr)
    | Lambda (Pats) (Expr)
    | Let (Decls) (Expr)
    | Lit (Literal)
    | App (Expr) (Exprs)
    | Paren (Expr)
    | Tuple (Exprs)
    | Var (Name)
    | Enum (Expr) (MaybeExpr) (MaybeExpr)
    | List (Exprs)
    | Neg (Expr)
    deriving (Data, Eq, Ord, Read, Show, Typeable)

-- Exprs -------------------------------------------------------
type Exprs = [Expr]

-- FunBind -----------------------------------------------------
data FunBind
    = FBHole (HoleID)
    | FunBind ((Maybe String)) (Name) (Pats) (Rhs)
    deriving (Data, Eq, Ord, Read, Show, Typeable)

-- FunBinds ----------------------------------------------------
type FunBinds = [FunBind]

-- GuardedExpr -------------------------------------------------
data GuardedExpr = GExpr (Expr) (Expr)
    deriving (Data, Eq, Ord, Read, Show, Typeable)

-- GuardedExprs ------------------------------------------------
type GuardedExprs = [GuardedExpr]

-- Literal -----------------------------------------------------
data Literal
    = LChar (Char)
    | LFloat (Float)
    | LInt (Int)
    | LString (String)
    deriving (Data, Eq, Ord, Read, Show, Typeable)

-- MaybeExpr ---------------------------------------------------
data MaybeExpr
    = NoExpr
    | JustExpr (Expr)
    deriving (Data, Eq, Ord, Read, Show, Typeable)

-- MaybeName ---------------------------------------------------
data MaybeName
    = NoName
    | JustName (Name)
    deriving (Data, Eq, Ord, Read, Show, Typeable)

-- Module ------------------------------------------------------
data Module = Module (MaybeName) (Body)
    deriving (Data, Eq, Ord, Read, Show, Typeable)

-- Name --------------------------------------------------------
data Name
    = Ident (String)
    | Operator (String)
    | Special (String)
    deriving (Data, Eq, Ord, Read, Show, Typeable)

-- Names -------------------------------------------------------
type Names = [Name]

-- Pat ---------------------------------------------------------
data Pat
    = PHole (HoleID)
    | PMultipleHole (HoleID)
    | PCon (Name) (Pats)
    | PInfixCon (Pat) (Name) (Pat)
    | PList (Pats)
    | PLit (Literal)
    | PParen (Pat)
    | PTuple (Pats)
    | PVar (Name)
    | PAs (Name) (Pat)
    | PWildcard
    deriving (Data, Eq, Ord, Read, Show, Typeable)

-- Pats --------------------------------------------------------
type Pats = [Pat]

-- RefactorChoice ----------------------------------------------
data RefactorChoice = RefactorChoice (String) (Expr)
    deriving (Data, Eq, Ord, Read, Show, Typeable)

-- RefactorChoices ---------------------------------------------
type RefactorChoices = [RefactorChoice]

-- Rhs ---------------------------------------------------------
data Rhs
    = Rhs (Expr) (Decls)
    | GRhs (GuardedExprs) (Decls)
    deriving (Data, Eq, Ord, Read, Show, Typeable)

-- GENERATED START

instance Uniplate Module where
    {-# INLINE uniplate #-}
    uniplate x = plate x

instance Uniplate Body where
    {-# INLINE uniplate #-}
    uniplate x = plate x

instance Uniplate Decl where
    {-# INLINE uniplate #-}
    uniplate (DFunBinds x1) = plate DFunBinds ||+ x1
    uniplate (DPatBind x1 x2) = plate (DPatBind x1) |+ x2
    uniplate x = plate x

instance Uniplate FunBind where
    {-# INLINE uniplate #-}
    uniplate (FunBind x1 x2 x3 x4) = plate (FunBind x1 x2 x3) |+ x4
    uniplate x = plate x

instance Uniplate Pat where
    {-# INLINE uniplate #-}
    uniplate (PCon x1 x2) = plate (PCon x1) ||* x2
    uniplate (PInfixCon x1 x2 x3) = plate PInfixCon |* x1 |- x2 |* x3
    uniplate (PList x1) = plate PList ||* x1
    uniplate (PParen x1) = plate PParen |* x1
    uniplate (PTuple x1) = plate PTuple ||* x1
    uniplate (PAs x1 x2) = plate (PAs x1) |* x2
    uniplate x = plate x

instance Uniplate [Pat] where
    {-# INLINE uniplate #-}
    uniplate ((:) x1 x2) = plate (:) |+ x1 |* x2
    uniplate x = plate x

instance Uniplate Rhs where
    {-# INLINE uniplate #-}
    uniplate (Rhs x1 x2) = plate Rhs |+ x1 ||+ x2
    uniplate (GRhs x1 x2) = plate GRhs ||+ x1 ||+ x2

instance Uniplate Expr where
    {-# INLINE uniplate #-}
    uniplate (Feedback x1 x2) = plate (Feedback x1) |* x2
    uniplate (MustUse x1) = plate MustUse |* x1
    uniplate (Eta x1 x2) = plate (Eta x1) |* x2
    uniplate (Refactor x1 x2) = plate Refactor |* x1 ||+ x2
    uniplate (Case x1 x2) = plate Case |* x1 ||+ x2
    uniplate (If x1 x2 x3) = plate If |* x1 |* x2 |* x3
    uniplate (InfixApp x1 x2 x3) = plate InfixApp |+ x1 |* x2 |+ x3
    uniplate (Lambda x1 x2) = plate (Lambda x1) |* x2
    uniplate (Let x1 x2) = plate Let ||+ x1 |* x2
    uniplate (App x1 x2) = plate App |* x1 ||* x2
    uniplate (Paren x1) = plate Paren |* x1
    uniplate (Tuple x1) = plate Tuple ||* x1
    uniplate (Enum x1 x2 x3) = plate Enum |* x1 |+ x2 |+ x3
    uniplate (List x1) = plate List ||* x1
    uniplate (Neg x1) = plate Neg |* x1
    uniplate x = plate x

instance Uniplate MaybeExpr where
    {-# INLINE uniplate #-}
    uniplate (JustExpr x1) = plate JustExpr |+ x1
    uniplate x = plate x

instance Uniplate Alt where
    {-# INLINE uniplate #-}
    uniplate (Alt x1 x2 x3) = plate (Alt x1 x2) |+ x3
    uniplate x = plate x

instance Uniplate Name where
    {-# INLINE uniplate #-}
    uniplate x = plate x

instance Uniplate MaybeName where
    {-# INLINE uniplate #-}
    uniplate x = plate x

instance Uniplate Literal where
    {-# INLINE uniplate #-}
    uniplate x = plate x

instance Uniplate GuardedExpr where
    {-# INLINE uniplate #-}
    uniplate (GExpr x1 x2) = plate GExpr |+ x1 |+ x2

instance Biplate Module Module where
    {-# INLINE biplate #-}
    biplate = plateSelf

instance Biplate Module Body where
    {-# INLINE biplate #-}
    biplate (Module x1 x2) = plate (Module x1) |* x2

instance Biplate Module Decl where
    {-# INLINE biplate #-}
    biplate (Module x1 x2) = plate (Module x1) |+ x2

instance Biplate Module FunBind where
    {-# INLINE biplate #-}
    biplate (Module x1 x2) = plate (Module x1) |+ x2

instance Biplate Module Pat where
    {-# INLINE biplate #-}
    biplate (Module x1 x2) = plate (Module x1) |+ x2

instance Biplate Module [Pat] where
    {-# INLINE biplate #-}
    biplate (Module x1 x2) = plate (Module x1) |+ x2

instance Biplate Module Rhs where
    {-# INLINE biplate #-}
    biplate (Module x1 x2) = plate (Module x1) |+ x2

instance Biplate Module Expr where
    {-# INLINE biplate #-}
    biplate (Module x1 x2) = plate (Module x1) |+ x2

instance Biplate Module MaybeExpr where
    {-# INLINE biplate #-}
    biplate (Module x1 x2) = plate (Module x1) |+ x2

instance Biplate Module Alt where
    {-# INLINE biplate #-}
    biplate (Module x1 x2) = plate (Module x1) |+ x2

instance Biplate Module Name where
    {-# INLINE biplate #-}
    biplate (Module x1 x2) = plate Module |+ x1 |+ x2

instance Biplate Module MaybeName where
    {-# INLINE biplate #-}
    biplate (Module x1 x2) = plate Module |* x1 |- x2

instance Biplate Module Literal where
    {-# INLINE biplate #-}
    biplate (Module x1 x2) = plate (Module x1) |+ x2

instance Biplate Module GuardedExpr where
    {-# INLINE biplate #-}
    biplate (Module x1 x2) = plate (Module x1) |+ x2

instance Biplate Body Module where
    {-# INLINE biplate #-}
    biplate x = plate x

instance Biplate Body Body where
    {-# INLINE biplate #-}
    biplate = plateSelf

instance Biplate Body Decl where
    {-# INLINE biplate #-}
    biplate (Body x1) = plate Body ||* x1
    biplate x = plate x

instance Biplate Body FunBind where
    {-# INLINE biplate #-}
    biplate (Body x1) = plate Body ||+ x1
    biplate x = plate x

instance Biplate Body Pat where
    {-# INLINE biplate #-}
    biplate (Body x1) = plate Body ||+ x1
    biplate x = plate x

instance Biplate Body [Pat] where
    {-# INLINE biplate #-}
    biplate (Body x1) = plate Body ||+ x1
    biplate x = plate x

instance Biplate Body Rhs where
    {-# INLINE biplate #-}
    biplate (Body x1) = plate Body ||+ x1
    biplate x = plate x

instance Biplate Body Expr where
    {-# INLINE biplate #-}
    biplate (Body x1) = plate Body ||+ x1
    biplate x = plate x

instance Biplate Body MaybeExpr where
    {-# INLINE biplate #-}
    biplate (Body x1) = plate Body ||+ x1
    biplate x = plate x

instance Biplate Body Alt where
    {-# INLINE biplate #-}
    biplate (Body x1) = plate Body ||+ x1
    biplate x = plate x

instance Biplate Body Name where
    {-# INLINE biplate #-}
    biplate (Body x1) = plate Body ||+ x1
    biplate x = plate x

instance Biplate Body MaybeName where
    {-# INLINE biplate #-}
    biplate x = plate x

instance Biplate Body Literal where
    {-# INLINE biplate #-}
    biplate (Body x1) = plate Body ||+ x1
    biplate x = plate x

instance Biplate Body GuardedExpr where
    {-# INLINE biplate #-}
    biplate (Body x1) = plate Body ||+ x1
    biplate x = plate x

instance Biplate Decl Module where
    {-# INLINE biplate #-}
    biplate x = plate x

instance Biplate Decl Body where
    {-# INLINE biplate #-}
    biplate x = plate x

instance Biplate Decl Decl where
    {-# INLINE biplate #-}
    biplate = plateSelf

instance Biplate Decl FunBind where
    {-# INLINE biplate #-}
    biplate (DFunBinds x1) = plate DFunBinds ||* x1
    biplate (DPatBind x1 x2) = plate (DPatBind x1) |+ x2
    biplate x = plate x

instance Biplate Decl Pat where
    {-# INLINE biplate #-}
    biplate (DFunBinds x1) = plate DFunBinds ||+ x1
    biplate (DPatBind x1 x2) = plate DPatBind |* x1 |+ x2
    biplate x = plate x

instance Biplate Decl [Pat] where
    {-# INLINE biplate #-}
    biplate (DFunBinds x1) = plate DFunBinds ||+ x1
    biplate (DPatBind x1 x2) = plate DPatBind |+ x1 |+ x2
    biplate x = plate x

instance Biplate Decl Rhs where
    {-# INLINE biplate #-}
    biplate (DFunBinds x1) = plate DFunBinds ||+ x1
    biplate (DPatBind x1 x2) = plate (DPatBind x1) |* x2
    biplate x = plate x

instance Biplate Decl Expr where
    {-# INLINE biplate #-}
    biplate (DFunBinds x1) = plate DFunBinds ||+ x1
    biplate (DPatBind x1 x2) = plate (DPatBind x1) |+ x2
    biplate x = plate x

instance Biplate Decl MaybeExpr where
    {-# INLINE biplate #-}
    biplate (DFunBinds x1) = plate DFunBinds ||+ x1
    biplate (DPatBind x1 x2) = plate (DPatBind x1) |+ x2
    biplate x = plate x

instance Biplate Decl Alt where
    {-# INLINE biplate #-}
    biplate (DFunBinds x1) = plate DFunBinds ||+ x1
    biplate (DPatBind x1 x2) = plate (DPatBind x1) |+ x2
    biplate x = plate x

instance Biplate Decl Name where
    {-# INLINE biplate #-}
    biplate (DFunBinds x1) = plate DFunBinds ||+ x1
    biplate (DPatBind x1 x2) = plate DPatBind |+ x1 |+ x2
    biplate x = plate x

instance Biplate Decl MaybeName where
    {-# INLINE biplate #-}
    biplate x = plate x

instance Biplate Decl Literal where
    {-# INLINE biplate #-}
    biplate (DFunBinds x1) = plate DFunBinds ||+ x1
    biplate (DPatBind x1 x2) = plate DPatBind |+ x1 |+ x2
    biplate x = plate x

instance Biplate Decl GuardedExpr where
    {-# INLINE biplate #-}
    biplate (DFunBinds x1) = plate DFunBinds ||+ x1
    biplate (DPatBind x1 x2) = plate (DPatBind x1) |+ x2
    biplate x = plate x

instance Biplate FunBind Module where
    {-# INLINE biplate #-}
    biplate x = plate x

instance Biplate FunBind Body where
    {-# INLINE biplate #-}
    biplate x = plate x

instance Biplate FunBind Decl where
    {-# INLINE biplate #-}
    biplate (FunBind x1 x2 x3 x4) = plate (FunBind x1 x2 x3) |+ x4
    biplate x = plate x

instance Biplate FunBind FunBind where
    {-# INLINE biplate #-}
    biplate = plateSelf

instance Biplate FunBind Pat where
    {-# INLINE biplate #-}
    biplate (FunBind x1 x2 x3 x4) = plate (FunBind x1 x2) ||* x3 |+ x4
    biplate x = plate x

instance Biplate FunBind [Pat] where
    {-# INLINE biplate #-}
    biplate (FunBind x1 x2 x3 x4) = plate (FunBind x1 x2) |* x3 |+ x4
    biplate x = plate x

instance Biplate FunBind Rhs where
    {-# INLINE biplate #-}
    biplate (FunBind x1 x2 x3 x4) = plate (FunBind x1 x2 x3) |* x4
    biplate x = plate x

instance Biplate FunBind Expr where
    {-# INLINE biplate #-}
    biplate (FunBind x1 x2 x3 x4) = plate (FunBind x1 x2 x3) |+ x4
    biplate x = plate x

instance Biplate FunBind MaybeExpr where
    {-# INLINE biplate #-}
    biplate (FunBind x1 x2 x3 x4) = plate (FunBind x1 x2 x3) |+ x4
    biplate x = plate x

instance Biplate FunBind Alt where
    {-# INLINE biplate #-}
    biplate (FunBind x1 x2 x3 x4) = plate (FunBind x1 x2 x3) |+ x4
    biplate x = plate x

instance Biplate FunBind Name where
    {-# INLINE biplate #-}
    biplate (FunBind x1 x2 x3 x4) =
        plate (FunBind x1) |* x2 ||+ x3 |+ x4
    biplate x = plate x

instance Biplate FunBind MaybeName where
    {-# INLINE biplate #-}
    biplate x = plate x

instance Biplate FunBind Literal where
    {-# INLINE biplate #-}
    biplate (FunBind x1 x2 x3 x4) = plate (FunBind x1 x2) ||+ x3 |+ x4
    biplate x = plate x

instance Biplate FunBind GuardedExpr where
    {-# INLINE biplate #-}
    biplate (FunBind x1 x2 x3 x4) = plate (FunBind x1 x2 x3) |+ x4
    biplate x = plate x

instance Biplate Pat Module where
    {-# INLINE biplate #-}
    biplate x = plate x

instance Biplate Pat Body where
    {-# INLINE biplate #-}
    biplate x = plate x

instance Biplate Pat Decl where
    {-# INLINE biplate #-}
    biplate x = plate x

instance Biplate Pat FunBind where
    {-# INLINE biplate #-}
    biplate x = plate x

instance Biplate Pat Pat where
    {-# INLINE biplate #-}
    biplate = plateSelf

instance Biplate Pat [Pat] where
    {-# INLINE biplate #-}
    biplate (PCon x1 x2) = plate (PCon x1) |* x2
    biplate (PInfixCon x1 x2 x3) = plate PInfixCon |+ x1 |- x2 |+ x3
    biplate (PList x1) = plate PList |* x1
    biplate (PParen x1) = plate PParen |+ x1
    biplate (PTuple x1) = plate PTuple |* x1
    biplate (PAs x1 x2) = plate (PAs x1) |+ x2
    biplate x = plate x

instance Biplate Pat Rhs where
    {-# INLINE biplate #-}
    biplate x = plate x

instance Biplate Pat Expr where
    {-# INLINE biplate #-}
    biplate x = plate x

instance Biplate Pat MaybeExpr where
    {-# INLINE biplate #-}
    biplate x = plate x

instance Biplate Pat Alt where
    {-# INLINE biplate #-}
    biplate x = plate x

instance Biplate Pat Name where
    {-# INLINE biplate #-}
    biplate (PCon x1 x2) = plate PCon |* x1 ||+ x2
    biplate (PInfixCon x1 x2 x3) = plate PInfixCon |+ x1 |* x2 |+ x3
    biplate (PList x1) = plate PList ||+ x1
    biplate (PParen x1) = plate PParen |+ x1
    biplate (PTuple x1) = plate PTuple ||+ x1
    biplate (PVar x1) = plate PVar |* x1
    biplate (PAs x1 x2) = plate PAs |* x1 |+ x2
    biplate x = plate x

instance Biplate Pat MaybeName where
    {-# INLINE biplate #-}
    biplate x = plate x

instance Biplate Pat Literal where
    {-# INLINE biplate #-}
    biplate (PCon x1 x2) = plate (PCon x1) ||+ x2
    biplate (PInfixCon x1 x2 x3) = plate PInfixCon |+ x1 |- x2 |+ x3
    biplate (PList x1) = plate PList ||+ x1
    biplate (PLit x1) = plate PLit |* x1
    biplate (PParen x1) = plate PParen |+ x1
    biplate (PTuple x1) = plate PTuple ||+ x1
    biplate (PAs x1 x2) = plate (PAs x1) |+ x2
    biplate x = plate x

instance Biplate Pat GuardedExpr where
    {-# INLINE biplate #-}
    biplate x = plate x

instance Biplate Rhs Module where
    {-# INLINE biplate #-}
    biplate x = plate x

instance Biplate Rhs Body where
    {-# INLINE biplate #-}
    biplate x = plate x

instance Biplate Rhs Decl where
    {-# INLINE biplate #-}
    biplate (Rhs x1 x2) = plate Rhs |+ x1 ||* x2
    biplate (GRhs x1 x2) = plate GRhs ||+ x1 ||* x2

instance Biplate Rhs FunBind where
    {-# INLINE biplate #-}
    biplate (Rhs x1 x2) = plate Rhs |+ x1 ||+ x2
    biplate (GRhs x1 x2) = plate GRhs ||+ x1 ||+ x2

instance Biplate Rhs Pat where
    {-# INLINE biplate #-}
    biplate (Rhs x1 x2) = plate Rhs |+ x1 ||+ x2
    biplate (GRhs x1 x2) = plate GRhs ||+ x1 ||+ x2

instance Biplate Rhs [Pat] where
    {-# INLINE biplate #-}
    biplate (Rhs x1 x2) = plate Rhs |+ x1 ||+ x2
    biplate (GRhs x1 x2) = plate GRhs ||+ x1 ||+ x2

instance Biplate Rhs Rhs where
    {-# INLINE biplate #-}
    biplate = plateSelf

instance Biplate Rhs Expr where
    {-# INLINE biplate #-}
    biplate (Rhs x1 x2) = plate Rhs |* x1 ||+ x2
    biplate (GRhs x1 x2) = plate GRhs ||+ x1 ||+ x2

instance Biplate Rhs MaybeExpr where
    {-# INLINE biplate #-}
    biplate (Rhs x1 x2) = plate Rhs |+ x1 ||+ x2
    biplate (GRhs x1 x2) = plate GRhs ||+ x1 ||+ x2

instance Biplate Rhs Alt where
    {-# INLINE biplate #-}
    biplate (Rhs x1 x2) = plate Rhs |+ x1 ||+ x2
    biplate (GRhs x1 x2) = plate GRhs ||+ x1 ||+ x2

instance Biplate Rhs Name where
    {-# INLINE biplate #-}
    biplate (Rhs x1 x2) = plate Rhs |+ x1 ||+ x2
    biplate (GRhs x1 x2) = plate GRhs ||+ x1 ||+ x2

instance Biplate Rhs MaybeName where
    {-# INLINE biplate #-}
    biplate x = plate x

instance Biplate Rhs Literal where
    {-# INLINE biplate #-}
    biplate (Rhs x1 x2) = plate Rhs |+ x1 ||+ x2
    biplate (GRhs x1 x2) = plate GRhs ||+ x1 ||+ x2

instance Biplate Rhs GuardedExpr where
    {-# INLINE biplate #-}
    biplate (Rhs x1 x2) = plate Rhs |+ x1 ||+ x2
    biplate (GRhs x1 x2) = plate GRhs ||* x1 ||+ x2

instance Biplate Expr Module where
    {-# INLINE biplate #-}
    biplate x = plate x

instance Biplate Expr Body where
    {-# INLINE biplate #-}
    biplate x = plate x

instance Biplate Expr Decl where
    {-# INLINE biplate #-}
    biplate (Feedback x1 x2) = plate (Feedback x1) |+ x2
    biplate (MustUse x1) = plate MustUse |+ x1
    biplate (Eta x1 x2) = plate (Eta x1) |+ x2
    biplate (Refactor x1 x2) = plate Refactor |+ x1 ||+ x2
    biplate (Case x1 x2) = plate Case |+ x1 ||+ x2
    biplate (If x1 x2 x3) = plate If |+ x1 |+ x2 |+ x3
    biplate (InfixApp x1 x2 x3) = plate InfixApp |+ x1 |+ x2 |+ x3
    biplate (Lambda x1 x2) = plate (Lambda x1) |+ x2
    biplate (Let x1 x2) = plate Let ||* x1 |+ x2
    biplate (App x1 x2) = plate App |+ x1 ||+ x2
    biplate (Paren x1) = plate Paren |+ x1
    biplate (Tuple x1) = plate Tuple ||+ x1
    biplate (Enum x1 x2 x3) = plate Enum |+ x1 |+ x2 |+ x3
    biplate (List x1) = plate List ||+ x1
    biplate (Neg x1) = plate Neg |+ x1
    biplate x = plate x

instance Biplate Expr FunBind where
    {-# INLINE biplate #-}
    biplate (Feedback x1 x2) = plate (Feedback x1) |+ x2
    biplate (MustUse x1) = plate MustUse |+ x1
    biplate (Eta x1 x2) = plate (Eta x1) |+ x2
    biplate (Refactor x1 x2) = plate Refactor |+ x1 ||+ x2
    biplate (Case x1 x2) = plate Case |+ x1 ||+ x2
    biplate (If x1 x2 x3) = plate If |+ x1 |+ x2 |+ x3
    biplate (InfixApp x1 x2 x3) = plate InfixApp |+ x1 |+ x2 |+ x3
    biplate (Lambda x1 x2) = plate (Lambda x1) |+ x2
    biplate (Let x1 x2) = plate Let ||+ x1 |+ x2
    biplate (App x1 x2) = plate App |+ x1 ||+ x2
    biplate (Paren x1) = plate Paren |+ x1
    biplate (Tuple x1) = plate Tuple ||+ x1
    biplate (Enum x1 x2 x3) = plate Enum |+ x1 |+ x2 |+ x3
    biplate (List x1) = plate List ||+ x1
    biplate (Neg x1) = plate Neg |+ x1
    biplate x = plate x

instance Biplate Expr Pat where
    {-# INLINE biplate #-}
    biplate (Feedback x1 x2) = plate (Feedback x1) |+ x2
    biplate (MustUse x1) = plate MustUse |+ x1
    biplate (Eta x1 x2) = plate (Eta x1) |+ x2
    biplate (Refactor x1 x2) = plate Refactor |+ x1 ||+ x2
    biplate (Case x1 x2) = plate Case |+ x1 ||+ x2
    biplate (If x1 x2 x3) = plate If |+ x1 |+ x2 |+ x3
    biplate (InfixApp x1 x2 x3) = plate InfixApp |+ x1 |+ x2 |+ x3
    biplate (Lambda x1 x2) = plate Lambda ||* x1 |+ x2
    biplate (Let x1 x2) = plate Let ||+ x1 |+ x2
    biplate (App x1 x2) = plate App |+ x1 ||+ x2
    biplate (Paren x1) = plate Paren |+ x1
    biplate (Tuple x1) = plate Tuple ||+ x1
    biplate (Enum x1 x2 x3) = plate Enum |+ x1 |+ x2 |+ x3
    biplate (List x1) = plate List ||+ x1
    biplate (Neg x1) = plate Neg |+ x1
    biplate x = plate x

instance Biplate Expr [Pat] where
    {-# INLINE biplate #-}
    biplate (Feedback x1 x2) = plate (Feedback x1) |+ x2
    biplate (MustUse x1) = plate MustUse |+ x1
    biplate (Eta x1 x2) = plate (Eta x1) |+ x2
    biplate (Refactor x1 x2) = plate Refactor |+ x1 ||+ x2
    biplate (Case x1 x2) = plate Case |+ x1 ||+ x2
    biplate (If x1 x2 x3) = plate If |+ x1 |+ x2 |+ x3
    biplate (InfixApp x1 x2 x3) = plate InfixApp |+ x1 |+ x2 |+ x3
    biplate (Lambda x1 x2) = plate Lambda |* x1 |+ x2
    biplate (Let x1 x2) = plate Let ||+ x1 |+ x2
    biplate (App x1 x2) = plate App |+ x1 ||+ x2
    biplate (Paren x1) = plate Paren |+ x1
    biplate (Tuple x1) = plate Tuple ||+ x1
    biplate (Enum x1 x2 x3) = plate Enum |+ x1 |+ x2 |+ x3
    biplate (List x1) = plate List ||+ x1
    biplate (Neg x1) = plate Neg |+ x1
    biplate x = plate x

instance Biplate Expr Rhs where
    {-# INLINE biplate #-}
    biplate (Feedback x1 x2) = plate (Feedback x1) |+ x2
    biplate (MustUse x1) = plate MustUse |+ x1
    biplate (Eta x1 x2) = plate (Eta x1) |+ x2
    biplate (Refactor x1 x2) = plate Refactor |+ x1 ||+ x2
    biplate (Case x1 x2) = plate Case |+ x1 ||+ x2
    biplate (If x1 x2 x3) = plate If |+ x1 |+ x2 |+ x3
    biplate (InfixApp x1 x2 x3) = plate InfixApp |+ x1 |+ x2 |+ x3
    biplate (Lambda x1 x2) = plate (Lambda x1) |+ x2
    biplate (Let x1 x2) = plate Let ||+ x1 |+ x2
    biplate (App x1 x2) = plate App |+ x1 ||+ x2
    biplate (Paren x1) = plate Paren |+ x1
    biplate (Tuple x1) = plate Tuple ||+ x1
    biplate (Enum x1 x2 x3) = plate Enum |+ x1 |+ x2 |+ x3
    biplate (List x1) = plate List ||+ x1
    biplate (Neg x1) = plate Neg |+ x1
    biplate x = plate x

instance Biplate Expr Expr where
    {-# INLINE biplate #-}
    biplate = plateSelf

instance Biplate Expr MaybeExpr where
    {-# INLINE biplate #-}
    biplate (Feedback x1 x2) = plate (Feedback x1) |+ x2
    biplate (MustUse x1) = plate MustUse |+ x1
    biplate (Eta x1 x2) = plate (Eta x1) |+ x2
    biplate (Refactor x1 x2) = plate Refactor |+ x1 ||+ x2
    biplate (Case x1 x2) = plate Case |+ x1 ||+ x2
    biplate (If x1 x2 x3) = plate If |+ x1 |+ x2 |+ x3
    biplate (InfixApp x1 x2 x3) = plate InfixApp |* x1 |+ x2 |* x3
    biplate (Lambda x1 x2) = plate (Lambda x1) |+ x2
    biplate (Let x1 x2) = plate Let ||+ x1 |+ x2
    biplate (App x1 x2) = plate App |+ x1 ||+ x2
    biplate (Paren x1) = plate Paren |+ x1
    biplate (Tuple x1) = plate Tuple ||+ x1
    biplate (Enum x1 x2 x3) = plate Enum |+ x1 |* x2 |* x3
    biplate (List x1) = plate List ||+ x1
    biplate (Neg x1) = plate Neg |+ x1
    biplate x = plate x

instance Biplate Expr Alt where
    {-# INLINE biplate #-}
    biplate (Feedback x1 x2) = plate (Feedback x1) |+ x2
    biplate (MustUse x1) = plate MustUse |+ x1
    biplate (Eta x1 x2) = plate (Eta x1) |+ x2
    biplate (Refactor x1 x2) = plate Refactor |+ x1 ||+ x2
    biplate (Case x1 x2) = plate Case |+ x1 ||* x2
    biplate (If x1 x2 x3) = plate If |+ x1 |+ x2 |+ x3
    biplate (InfixApp x1 x2 x3) = plate InfixApp |+ x1 |+ x2 |+ x3
    biplate (Lambda x1 x2) = plate (Lambda x1) |+ x2
    biplate (Let x1 x2) = plate Let ||+ x1 |+ x2
    biplate (App x1 x2) = plate App |+ x1 ||+ x2
    biplate (Paren x1) = plate Paren |+ x1
    biplate (Tuple x1) = plate Tuple ||+ x1
    biplate (Enum x1 x2 x3) = plate Enum |+ x1 |+ x2 |+ x3
    biplate (List x1) = plate List ||+ x1
    biplate (Neg x1) = plate Neg |+ x1
    biplate x = plate x

instance Biplate Expr Name where
    {-# INLINE biplate #-}
    biplate (Feedback x1 x2) = plate (Feedback x1) |+ x2
    biplate (MustUse x1) = plate MustUse |+ x1
    biplate (Eta x1 x2) = plate (Eta x1) |+ x2
    biplate (Refactor x1 x2) = plate Refactor |+ x1 ||+ x2
    biplate (Case x1 x2) = plate Case |+ x1 ||+ x2
    biplate (Con x1) = plate Con |* x1
    biplate (If x1 x2 x3) = plate If |+ x1 |+ x2 |+ x3
    biplate (InfixApp x1 x2 x3) = plate InfixApp |+ x1 |+ x2 |+ x3
    biplate (Lambda x1 x2) = plate Lambda ||+ x1 |+ x2
    biplate (Let x1 x2) = plate Let ||+ x1 |+ x2
    biplate (App x1 x2) = plate App |+ x1 ||+ x2
    biplate (Paren x1) = plate Paren |+ x1
    biplate (Tuple x1) = plate Tuple ||+ x1
    biplate (Var x1) = plate Var |* x1
    biplate (Enum x1 x2 x3) = plate Enum |+ x1 |+ x2 |+ x3
    biplate (List x1) = plate List ||+ x1
    biplate (Neg x1) = plate Neg |+ x1
    biplate x = plate x

instance Biplate Expr MaybeName where
    {-# INLINE biplate #-}
    biplate x = plate x

instance Biplate Expr Literal where
    {-# INLINE biplate #-}
    biplate (Feedback x1 x2) = plate (Feedback x1) |+ x2
    biplate (MustUse x1) = plate MustUse |+ x1
    biplate (Eta x1 x2) = plate (Eta x1) |+ x2
    biplate (Refactor x1 x2) = plate Refactor |+ x1 ||+ x2
    biplate (Case x1 x2) = plate Case |+ x1 ||+ x2
    biplate (If x1 x2 x3) = plate If |+ x1 |+ x2 |+ x3
    biplate (InfixApp x1 x2 x3) = plate InfixApp |+ x1 |+ x2 |+ x3
    biplate (Lambda x1 x2) = plate Lambda ||+ x1 |+ x2
    biplate (Let x1 x2) = plate Let ||+ x1 |+ x2
    biplate (Lit x1) = plate Lit |* x1
    biplate (App x1 x2) = plate App |+ x1 ||+ x2
    biplate (Paren x1) = plate Paren |+ x1
    biplate (Tuple x1) = plate Tuple ||+ x1
    biplate (Enum x1 x2 x3) = plate Enum |+ x1 |+ x2 |+ x3
    biplate (List x1) = plate List ||+ x1
    biplate (Neg x1) = plate Neg |+ x1
    biplate x = plate x

instance Biplate Expr GuardedExpr where
    {-# INLINE biplate #-}
    biplate (Feedback x1 x2) = plate (Feedback x1) |+ x2
    biplate (MustUse x1) = plate MustUse |+ x1
    biplate (Eta x1 x2) = plate (Eta x1) |+ x2
    biplate (Refactor x1 x2) = plate Refactor |+ x1 ||+ x2
    biplate (Case x1 x2) = plate Case |+ x1 ||+ x2
    biplate (If x1 x2 x3) = plate If |+ x1 |+ x2 |+ x3
    biplate (InfixApp x1 x2 x3) = plate InfixApp |+ x1 |+ x2 |+ x3
    biplate (Lambda x1 x2) = plate (Lambda x1) |+ x2
    biplate (Let x1 x2) = plate Let ||+ x1 |+ x2
    biplate (App x1 x2) = plate App |+ x1 ||+ x2
    biplate (Paren x1) = plate Paren |+ x1
    biplate (Tuple x1) = plate Tuple ||+ x1
    biplate (Enum x1 x2 x3) = plate Enum |+ x1 |+ x2 |+ x3
    biplate (List x1) = plate List ||+ x1
    biplate (Neg x1) = plate Neg |+ x1
    biplate x = plate x

instance Biplate MaybeExpr Module where
    {-# INLINE biplate #-}
    biplate x = plate x

instance Biplate MaybeExpr Body where
    {-# INLINE biplate #-}
    biplate x = plate x

instance Biplate MaybeExpr Decl where
    {-# INLINE biplate #-}
    biplate (JustExpr x1) = plate JustExpr |+ x1
    biplate x = plate x

instance Biplate MaybeExpr FunBind where
    {-# INLINE biplate #-}
    biplate (JustExpr x1) = plate JustExpr |+ x1
    biplate x = plate x

instance Biplate MaybeExpr Pat where
    {-# INLINE biplate #-}
    biplate (JustExpr x1) = plate JustExpr |+ x1
    biplate x = plate x

instance Biplate MaybeExpr [Pat] where
    {-# INLINE biplate #-}
    biplate (JustExpr x1) = plate JustExpr |+ x1
    biplate x = plate x

instance Biplate MaybeExpr Rhs where
    {-# INLINE biplate #-}
    biplate (JustExpr x1) = plate JustExpr |+ x1
    biplate x = plate x

instance Biplate MaybeExpr Expr where
    {-# INLINE biplate #-}
    biplate (JustExpr x1) = plate JustExpr |* x1
    biplate x = plate x

instance Biplate MaybeExpr MaybeExpr where
    {-# INLINE biplate #-}
    biplate = plateSelf

instance Biplate MaybeExpr Alt where
    {-# INLINE biplate #-}
    biplate (JustExpr x1) = plate JustExpr |+ x1
    biplate x = plate x

instance Biplate MaybeExpr Name where
    {-# INLINE biplate #-}
    biplate (JustExpr x1) = plate JustExpr |+ x1
    biplate x = plate x

instance Biplate MaybeExpr MaybeName where
    {-# INLINE biplate #-}
    biplate x = plate x

instance Biplate MaybeExpr Literal where
    {-# INLINE biplate #-}
    biplate (JustExpr x1) = plate JustExpr |+ x1
    biplate x = plate x

instance Biplate MaybeExpr GuardedExpr where
    {-# INLINE biplate #-}
    biplate (JustExpr x1) = plate JustExpr |+ x1
    biplate x = plate x

instance Biplate Alt Module where
    {-# INLINE biplate #-}
    biplate x = plate x

instance Biplate Alt Body where
    {-# INLINE biplate #-}
    biplate x = plate x

instance Biplate Alt Decl where
    {-# INLINE biplate #-}
    biplate (Alt x1 x2 x3) = plate (Alt x1 x2) |+ x3
    biplate x = plate x

instance Biplate Alt FunBind where
    {-# INLINE biplate #-}
    biplate (Alt x1 x2 x3) = plate (Alt x1 x2) |+ x3
    biplate x = plate x

instance Biplate Alt Pat where
    {-# INLINE biplate #-}
    biplate (Alt x1 x2 x3) = plate (Alt x1) |* x2 |+ x3
    biplate x = plate x

instance Biplate Alt [Pat] where
    {-# INLINE biplate #-}
    biplate (Alt x1 x2 x3) = plate (Alt x1) |+ x2 |+ x3
    biplate x = plate x

instance Biplate Alt Rhs where
    {-# INLINE biplate #-}
    biplate (Alt x1 x2 x3) = plate (Alt x1 x2) |* x3
    biplate x = plate x

instance Biplate Alt Expr where
    {-# INLINE biplate #-}
    biplate (Alt x1 x2 x3) = plate (Alt x1 x2) |+ x3
    biplate x = plate x

instance Biplate Alt MaybeExpr where
    {-# INLINE biplate #-}
    biplate (Alt x1 x2 x3) = plate (Alt x1 x2) |+ x3
    biplate x = plate x

instance Biplate Alt Alt where
    {-# INLINE biplate #-}
    biplate = plateSelf

instance Biplate Alt Name where
    {-# INLINE biplate #-}
    biplate (Alt x1 x2 x3) = plate (Alt x1) |+ x2 |+ x3
    biplate x = plate x

instance Biplate Alt MaybeName where
    {-# INLINE biplate #-}
    biplate x = plate x

instance Biplate Alt Literal where
    {-# INLINE biplate #-}
    biplate (Alt x1 x2 x3) = plate (Alt x1) |+ x2 |+ x3
    biplate x = plate x

instance Biplate Alt GuardedExpr where
    {-# INLINE biplate #-}
    biplate (Alt x1 x2 x3) = plate (Alt x1 x2) |+ x3
    biplate x = plate x

instance Biplate Name Module where
    {-# INLINE biplate #-}
    biplate x = plate x

instance Biplate Name Body where
    {-# INLINE biplate #-}
    biplate x = plate x

instance Biplate Name Decl where
    {-# INLINE biplate #-}
    biplate x = plate x

instance Biplate Name FunBind where
    {-# INLINE biplate #-}
    biplate x = plate x

instance Biplate Name Pat where
    {-# INLINE biplate #-}
    biplate x = plate x

instance Biplate Name [Pat] where
    {-# INLINE biplate #-}
    biplate x = plate x

instance Biplate Name Rhs where
    {-# INLINE biplate #-}
    biplate x = plate x

instance Biplate Name Expr where
    {-# INLINE biplate #-}
    biplate x = plate x

instance Biplate Name MaybeExpr where
    {-# INLINE biplate #-}
    biplate x = plate x

instance Biplate Name Alt where
    {-# INLINE biplate #-}
    biplate x = plate x

instance Biplate Name Name where
    {-# INLINE biplate #-}
    biplate = plateSelf

instance Biplate Name MaybeName where
    {-# INLINE biplate #-}
    biplate x = plate x

instance Biplate Name Literal where
    {-# INLINE biplate #-}
    biplate x = plate x

instance Biplate Name GuardedExpr where
    {-# INLINE biplate #-}
    biplate x = plate x

instance Biplate MaybeName Module where
    {-# INLINE biplate #-}
    biplate x = plate x

instance Biplate MaybeName Body where
    {-# INLINE biplate #-}
    biplate x = plate x

instance Biplate MaybeName Decl where
    {-# INLINE biplate #-}
    biplate x = plate x

instance Biplate MaybeName FunBind where
    {-# INLINE biplate #-}
    biplate x = plate x

instance Biplate MaybeName Pat where
    {-# INLINE biplate #-}
    biplate x = plate x

instance Biplate MaybeName [Pat] where
    {-# INLINE biplate #-}
    biplate x = plate x

instance Biplate MaybeName Rhs where
    {-# INLINE biplate #-}
    biplate x = plate x

instance Biplate MaybeName Expr where
    {-# INLINE biplate #-}
    biplate x = plate x

instance Biplate MaybeName MaybeExpr where
    {-# INLINE biplate #-}
    biplate x = plate x

instance Biplate MaybeName Alt where
    {-# INLINE biplate #-}
    biplate x = plate x

instance Biplate MaybeName Name where
    {-# INLINE biplate #-}
    biplate (JustName x1) = plate JustName |* x1
    biplate x = plate x

instance Biplate MaybeName MaybeName where
    {-# INLINE biplate #-}
    biplate = plateSelf

instance Biplate MaybeName Literal where
    {-# INLINE biplate #-}
    biplate x = plate x

instance Biplate MaybeName GuardedExpr where
    {-# INLINE biplate #-}
    biplate x = plate x

instance Biplate Literal Module where
    {-# INLINE biplate #-}
    biplate x = plate x

instance Biplate Literal Body where
    {-# INLINE biplate #-}
    biplate x = plate x

instance Biplate Literal Decl where
    {-# INLINE biplate #-}
    biplate x = plate x

instance Biplate Literal FunBind where
    {-# INLINE biplate #-}
    biplate x = plate x

instance Biplate Literal Pat where
    {-# INLINE biplate #-}
    biplate x = plate x

instance Biplate Literal [Pat] where
    {-# INLINE biplate #-}
    biplate x = plate x

instance Biplate Literal Rhs where
    {-# INLINE biplate #-}
    biplate x = plate x

instance Biplate Literal Expr where
    {-# INLINE biplate #-}
    biplate x = plate x

instance Biplate Literal MaybeExpr where
    {-# INLINE biplate #-}
    biplate x = plate x

instance Biplate Literal Alt where
    {-# INLINE biplate #-}
    biplate x = plate x

instance Biplate Literal Name where
    {-# INLINE biplate #-}
    biplate x = plate x

instance Biplate Literal MaybeName where
    {-# INLINE biplate #-}
    biplate x = plate x

instance Biplate Literal Literal where
    {-# INLINE biplate #-}
    biplate = plateSelf

instance Biplate Literal GuardedExpr where
    {-# INLINE biplate #-}
    biplate x = plate x

instance Biplate GuardedExpr Module where
    {-# INLINE biplate #-}
    biplate x = plate x

instance Biplate GuardedExpr Body where
    {-# INLINE biplate #-}
    biplate x = plate x

instance Biplate GuardedExpr Decl where
    {-# INLINE biplate #-}
    biplate (GExpr x1 x2) = plate GExpr |+ x1 |+ x2

instance Biplate GuardedExpr FunBind where
    {-# INLINE biplate #-}
    biplate (GExpr x1 x2) = plate GExpr |+ x1 |+ x2

instance Biplate GuardedExpr Pat where
    {-# INLINE biplate #-}
    biplate (GExpr x1 x2) = plate GExpr |+ x1 |+ x2

instance Biplate GuardedExpr [Pat] where
    {-# INLINE biplate #-}
    biplate (GExpr x1 x2) = plate GExpr |+ x1 |+ x2

instance Biplate GuardedExpr Rhs where
    {-# INLINE biplate #-}
    biplate (GExpr x1 x2) = plate GExpr |+ x1 |+ x2

instance Biplate GuardedExpr Expr where
    {-# INLINE biplate #-}
    biplate (GExpr x1 x2) = plate GExpr |* x1 |* x2

instance Biplate GuardedExpr MaybeExpr where
    {-# INLINE biplate #-}
    biplate (GExpr x1 x2) = plate GExpr |+ x1 |+ x2

instance Biplate GuardedExpr Alt where
    {-# INLINE biplate #-}
    biplate (GExpr x1 x2) = plate GExpr |+ x1 |+ x2

instance Biplate GuardedExpr Name where
    {-# INLINE biplate #-}
    biplate (GExpr x1 x2) = plate GExpr |+ x1 |+ x2

instance Biplate GuardedExpr MaybeName where
    {-# INLINE biplate #-}
    biplate x = plate x

instance Biplate GuardedExpr Literal where
    {-# INLINE biplate #-}
    biplate (GExpr x1 x2) = plate GExpr |+ x1 |+ x2

instance Biplate GuardedExpr GuardedExpr where
    {-# INLINE biplate #-}
    biplate = plateSelf

instance Biplate RefactorChoice Module where
    {-# INLINE biplate #-}
    biplate x = plate x

instance Biplate RefactorChoice Body where
    {-# INLINE biplate #-}
    biplate x = plate x

instance Biplate RefactorChoice Decl where
    {-# INLINE biplate #-}
    biplate (RefactorChoice x1 x2) = plate (RefactorChoice x1) |+ x2

instance Biplate RefactorChoice FunBind where
    {-# INLINE biplate #-}
    biplate (RefactorChoice x1 x2) = plate (RefactorChoice x1) |+ x2

instance Biplate RefactorChoice Pat where
    {-# INLINE biplate #-}
    biplate (RefactorChoice x1 x2) = plate (RefactorChoice x1) |+ x2

instance Biplate RefactorChoice [Pat] where
    {-# INLINE biplate #-}
    biplate (RefactorChoice x1 x2) = plate (RefactorChoice x1) |+ x2

instance Biplate RefactorChoice Rhs where
    {-# INLINE biplate #-}
    biplate (RefactorChoice x1 x2) = plate (RefactorChoice x1) |+ x2

instance Biplate RefactorChoice Expr where
    {-# INLINE biplate #-}
    biplate (RefactorChoice x1 x2) = plate (RefactorChoice x1) |* x2

instance Biplate RefactorChoice MaybeExpr where
    {-# INLINE biplate #-}
    biplate (RefactorChoice x1 x2) = plate (RefactorChoice x1) |+ x2

instance Biplate RefactorChoice Alt where
    {-# INLINE biplate #-}
    biplate (RefactorChoice x1 x2) = plate (RefactorChoice x1) |+ x2

instance Biplate RefactorChoice Name where
    {-# INLINE biplate #-}
    biplate (RefactorChoice x1 x2) = plate (RefactorChoice x1) |+ x2

instance Biplate RefactorChoice MaybeName where
    {-# INLINE biplate #-}
    biplate x = plate x

instance Biplate RefactorChoice Literal where
    {-# INLINE biplate #-}
    biplate (RefactorChoice x1 x2) = plate (RefactorChoice x1) |+ x2

instance Biplate RefactorChoice GuardedExpr where
    {-# INLINE biplate #-}
    biplate (RefactorChoice x1 x2) = plate (RefactorChoice x1) |+ x2

-- GENERATED STOP

-- {-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE GADTs #-}
-- {-# LANGUAGE DataKinds #-}
-- {-# LANGUAGE FlexibleContexts #-}
-- {-# OPTIONS_GHC -Wno-orphans #-}

-- module GhcLib.Utility.ShowCore where
-- -- | Module containing Show instances for GHC types
-- --  CoreSyn and HsSyn 

-- import GHC
-- import GHC.Core.DataCon (DataCon(..), mkDataCon, dataConName)
-- import GHC.Types.Var (Var(..), TyCoVarBinder(..), VarBndr(..), ArgFlag(..), AnonArgFlag, TyCoVar, Specificity(..))
-- import GHC.Types.Literal (Literal(..), LitNumType, pprLiteral)
-- import GHC.Types.Basic
--     ( FunctionOrData(IsFunction, IsData),
--       PromotionFlag(..),
--       RecFlag,
--       Origin,
--       Boxity )
-- import GHC.Types.Tickish ( CoreTickish )
-- import GHC.Core.TyCon (TyCon(..))
-- import GHC.Core.TyCo.Rep (Type(..), CoercionR, TyLit(..), TyCoBinder)
-- import GHC.Types.Name (Name(..),
--                       isHoleName,
--                       nameStableString,
--                       OccName,
--                       getSrcLoc,
--                       NamedThing (getName),
--                       pprOccName,
--                       pprDefinedAt,
--                       getOccString,
--                       nameUnique,
--                       HasOccName (occName),
--                       isDataConName,
--                       isTyConName,
--                       isTyVarName,
--                       isSystemName)
-- import GHC.Tc.Errors.Hole.FitTypes (TypedHole(..), HoleFitPlugin(..), FitPlugin(..), CandPlugin(..))
-- import GHC.Tc.Types.Constraint (Hole(..), HoleSort(..), Implication, CtLoc, Cts)
-- import GHC.Tc.Types.Evidence (HoleExprRef (..), HsWrapper, TcEvBinds, QuoteWrapper)
-- import GHC.Types.Unique.Set (UniqSet, pprUniqSet)
-- import qualified GHC.Core as GHC
-- import GHC.Unit.Module.Warnings (Warnings(..), pprWarningTxtForMsg, WarningTxt)
-- import GHC.Types.TyThing (TyThing(..))
-- import GHC.Core.Coercion.Axiom (CoAxiom)
-- import GHC.Core.ConLike (ConLike)
-- import GHC.Utils.Outputable (showSDocUnsafe, Outputable (ppr))
-- import GHC.Utils.Encoding (utf8DecodeByteString)
-- import GHC.Driver.Session (DynFlags(..), GhcNameVersion, PlatformMisc)
-- import GHC.Unit.Types (GenModule(..), Unit(..))
-- import GHC.Data.EnumSet (EnumSet, toList)
-- import GHC.Driver.Flags (GeneralFlag(..))
-- import Data.List (intersperse, intercalate)

-- -- == for hsSyn == 
-- import GHC.Data.Bag (Bag(..), bagToList, unitBag, isEmptyBag)
-- import Data.Void (Void)
-- import Data.Generics.Uniplate.Data ( Biplate )

-- -- ===============

-- -- | a haskell project is a list of core binds, type CoreProgram = [CoreBind], type CoreBind = Bind CoreBndr
-- instance Show bndr => Show (GHC.Bind bndr) where
--     show :: Show bndr => GHC.Bind bndr -> String
--     show (GHC.NonRec bndr e) = show bndr ++ " = " ++ show e
--     show (GHC.Rec bs) = show bs

-- instance Show bndr => Show (GHC.Expr bndr) where
--   show :: Show bndr => GHC.Expr bndr -> String
--   show (GHC.Var var) = "Var : " ++ show var
--   show (GHC.Lit  lit) = "Lit : " ++ show lit
--   show (GHC.App expr args) = encloseP $ "App : " ++ show expr ++  encloseP (show args)
--   show (GHC.Lam b expr) = "Lam : " ++ show b ++ " -> " ++ show expr
--   show (GHC.Let b expr) = "Let : " ++ show b ++ " in " ++ show expr
--   show (GHC.Case scru bndr retTyp alts) = "Case : " ++ show scru ++ " of " ++ encloseP ("Binder " ++ show bndr) ++ encloseP ("RetType " ++ show retTyp) ++  encloseP ("Alts "  ++ show alts)
--   show (GHC.Cast expr coercion) = undefined
--   show (GHC.Tick tick expr) =  show expr 
--   show expr@(GHC.Type typ) = "Typ : " ++ show typ
--   show expr@(GHC.Coercion coercion) = undefined


-- encloseP :: String -> String
-- encloseP s = " (" ++ s ++ ") "


-- instance Show Var where
--   show :: Var -> String
--   --show x = showSDocUnsafe $ ppr (varName x)  -- show uniques aswell
--   show = getOccString  -- show occurence name 

-- instance Show Literal where
--   show :: Literal -> String
--   show (LitString l)    = utf8DecodeByteString l
--   show (LitChar c)      = [c]
--   show (LitNumber _ i)  = show i
--   -- ? It could be better if we could show rest of the literals aswell
--   show _                = "Lit"

-- -- * Deriving Show instances for Type
-- instance Show Type where
--   show :: Type -> String
--   show (TyVarTy v)         = encloseP $ "TyVarTy " ++ show v
--   show (AppTy t1 t2)       = encloseP $ "AppTy " ++ show t1 ++ " " ++ show t2
--   show (TyConApp t tks)    = encloseP $ "TyConApp " ++ show t ++ concatMap show tks
--   show (ForAllTy _ t)      = encloseP $ "ForAllTy " ++ show t
--   show (FunTy _ _ arg res) = encloseP $ "FunTy " ++ show arg ++ " -> " ++ show res
--   show (LitTy tl)          = encloseP $ "LitTy " ++ show tl
--   show (CastTy t _)        = encloseP $ "CastTy " ++ show t
--   show (CoercionTy c)      = encloseP $ "CoercionTy " ++ show c

-- instance Show TyCon where
--   show :: TyCon -> String
--   show t = getOccString $ tyConName t

-- deriving stock instance Show TyLit


-- instance Show CoercionR where
--   show :: CoercionR -> String
--   show = showSDocUnsafe . ppr


-- -- * Deriving Show instances for Alts
-- deriving stock instance Show b => Show (GHC.Alt b)
-- deriving stock instance Show GHC.AltCon

-- instance Show DataCon where
--   --show d = "DCon: " ++ showSDocUnsafe (ppr d)  -- show both name and unique
--   show :: DataCon -> String
--   show d = showSDocUnsafe (ppr d)
--     --name2Str (dataConName d) 


-- -- * Deriving Show instance for Tick
-- instance Show CoreTickish where
--   show = showSDocUnsafe . ppr


{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module GhcLib.Utility.ShowCore where
-- | Module containing Show instances for GHC types
--  CoreSyn and HsSyn 

import GHC
import GHC.Core.DataCon (DataCon(..), mkDataCon, dataConName)
import GHC.Types.Var (Var(..), TyCoVarBinder(..), VarBndr(..), ArgFlag(..), AnonArgFlag, TyCoVar, Specificity(..))
import GHC.Types.Literal (Literal(..), LitNumType, pprLiteral)
import GHC.Types.Basic
    ( FunctionOrData(IsFunction, IsData),
      PromotionFlag(..),
      RecFlag,
      Origin,
      Boxity )
import GHC.Types.Tickish ( CoreTickish )
import GHC.Core.TyCon (TyCon(..))
import GHC.Core.TyCo.Rep (Type(..), CoercionR, TyLit(..), TyCoBinder)
import GHC.Types.Name (Name(..),
                      isHoleName,
                      nameStableString,
                      OccName,
                      getSrcLoc,
                      NamedThing (getName),
                      pprOccName,
                      pprDefinedAt,
                      getOccString,
                      nameUnique,
                      HasOccName (occName),
                      isDataConName,
                      isTyConName,
                      isTyVarName,
                      isSystemName)
import GHC.Tc.Errors.Hole.FitTypes (TypedHole(..), HoleFitPlugin(..), FitPlugin(..), CandPlugin(..))
import GHC.Tc.Types.Constraint (Hole(..), HoleSort(..), Implication, CtLoc, Cts)
import GHC.Tc.Types.Evidence (HoleExprRef (..), HsWrapper, TcEvBinds, QuoteWrapper)
import GHC.Types.Unique.Set (UniqSet, pprUniqSet)
import GHC.Core (Bind(..), Expr(..), Alt(..), AltCon(..), CoreProgram, CoreBind, CoreExpr)
import GHC.Unit.Module.Warnings (Warnings(..), pprWarningTxtForMsg, WarningTxt)
import GHC.Types.TyThing (TyThing(..))
import GHC.Core.Coercion.Axiom (CoAxiom)
import GHC.Core.ConLike (ConLike)
import GHC.Utils.Outputable (showSDocUnsafe, Outputable (ppr))
import GHC.Utils.Encoding (utf8DecodeByteString)
import GHC.Driver.Session (DynFlags(..), GhcNameVersion, PlatformMisc)
import GHC.Unit.Types (GenModule(..), Unit(..))
import GHC.Data.EnumSet (EnumSet, toList)
import GHC.Driver.Flags (GeneralFlag(..))
import Data.List (intersperse, intercalate)

-- == for hsSyn == 
import GHC.Data.Bag (Bag(..), bagToList, unitBag, isEmptyBag)
import Data.Void (Void)
import Data.Generics.Uniplate.Data ( Biplate )
-- ===============

-- * Show Instance are declared in the order of the data type declaration
-- | a haskell project is a list of core binds, type CoreProgram = [CoreBind], type CoreBind = Bind CoreBndr
deriving stock instance Show bndr => Show (Expr bndr)

-- * Deriving Show instance for Var Id 
instance Show Var where
  show :: Var -> String
  show x = showSDocUnsafe $ ppr (varName x)  -- show uniques aswell
  -- show = getOccString  -- show occurence name


-- * Deriving Show instance for Lit 
instance Show Literal where
  show :: Literal -> String
  show (LitString l)    = utf8DecodeByteString l
  show (LitChar c)      =  [c]
  show (LitNumber _ i)  = show i
  show l                = "lit"

-- * Deriving Show instance for Let
deriving stock instance Show bndr => Show (Bind bndr)

-- * Deriving Show instance for Case
deriving stock instance Show bndr => Show (Alt bndr)
deriving stock instance Show AltCon
instance Show DataCon where
  --show d = "DCon: " ++ showSDocUnsafe (ppr d)  -- show both name and unique
  show :: DataCon -> String
  show d = showSDocUnsafe (ppr d)
    --name2Str (dataConName d) 

-- * Deriving Show instance for Type 
deriving stock instance Show Type

instance Show TyCon where
  show :: TyCon -> String
  show t = getOccString $ tyConName t
  
deriving stock instance (Show a, Show b) => Show (VarBndr a b)

deriving stock instance Show ArgFlag

deriving stock instance Show Specificity

instance Show AnonArgFlag where
  show :: AnonArgFlag -> String
  show = showSDocUnsafe . ppr

deriving stock instance Show TyLit

-- * Deriving Show instance for coercion
instance Show CoreTickish where
  show :: CoreTickish -> String
  show = showSDocUnsafe . ppr

instance Show CoercionR where
  show :: CoercionR -> String
  show = showSDocUnsafe . ppr


-- deriving stock instance Show HoleFitPlugin
-- deriving stock instance Show Hole
-- deriving stock instance Show HoleSort

-- instance Show FitPlugin where
--   show fp = "FitPlugin"

-- instance Show Implication where
--   show i = "Implication"

-- instance Show OccName where
--   show n = showSDocUnsafe $ pprOccName n

-- instance Show CandPlugin where
-- deriving stock instance Show HoleFitPlugin
--   show c = "CandPlugin"

-- instance Show HoleExprRef where
--   show (HER e t u) = "(HER " ++ "evTerm " ++ "type: " ++ show t  ++ " uq: " ++ show u ++ ")"

-- instance Show CtLoc where
--   show c = "CtLoc"

-- {- instance Show VarSet where 
--   show x = showSDocUnsafe (pprVarSet x)  -}

-- instance Show a => Show (UniqSet a) where
--   show x = showSDocUnsafe (pprUniqSet undefined x)






-- --deriving instance Show Literal




-- instance Show TyCon where
--   show t = name2Str $ tyConName t




-- deriving instance Show Type



-- {- instance Show Type where 
--   show (TyVarTy v)         = p $ "TyVarTy " ++ show v 
--   show (AppTy t1 t2)       = p $ "AppTy " ++ show t1 ++ " " ++ show t2 
--   show (TyConApp t tks)    = p $ "TyConApp " ++ show t ++ concatMap show tks 
--   show (ForAllTy _ t)      = p $ "ForAllTy " ++ show t 
--   show (FunTy _ _ arg res) = p $ "FunTy " ++ show arg ++ " -> " ++ show res 
--   show (LitTy tl)          = p $ "LitTy " ++ show tl 
--   show (CastTy t _)        = p $ "CastTy " ++ show t 
--   show (CoercionTy c)      = p $ "CoercionTy " ++ show c  -}

-- p :: [Char] -> [Char]
-- p x = "(" ++ x ++ ")"



-- instance Show FunctionOrData where
--   show IsFunction = "(function)"
--   show IsData     = "(data)"


-- instance Show LitNumType where
--   show :: LitNumType -> String
--   show _ = "Int " -- Litnum types are Int/Nat/Words of different byte sizes 



-- -- show occName (not unique)
-- name2Str :: Name -> String
-- name2Str = getOccString

-- instance Show Warnings where
--   show :: Warnings -> String
--   show (NoWarnings)  = "No warning"
--   show (WarnAll w)   = showSDocUnsafe $ pprWarningTxtForMsg w
--   show (WarnSome ws) = concatMap (\(oc,wt) -> show oc ++ showSDocUnsafe (pprWarningTxtForMsg wt)) ws

-- deriving instance Show TyThing

-- instance Show (CoAxiom a) where
--   show = showSDocUnsafe . ppr

-- instance Show ConLike where
--   show = showSDocUnsafe . ppr

-- instance Show DynFlags where
--   show (DynFlags {generalFlags = g}) = show g


-- instance Show (EnumSet GeneralFlag) where
--   show e = intercalate ", \n" (map show (toList e))

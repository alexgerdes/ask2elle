{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module GhcLib.GHCRelated.ShowCore where

-- \| Module containing Show instances for GHC types
--  CoreSyn and HsSyn

import GHC
import GHC.Core
    ( Alt (..)
    , AltCon (..)
    , Bind (..)
    , CoreBind
    , CoreExpr
    , CoreProgram
    , Expr (..)
    )
import GHC.Core.Coercion.Axiom (CoAxiom)
import GHC.Core.ConLike (ConLike)
import GHC.Core.DataCon (DataCon (..), dataConName, mkDataCon)
import GHC.Core.TyCo.Rep (CoercionR, TyLit (..), Type (..))
import GHC.Core.TyCon (TyCon (..))
import GHC.Data.EnumSet (EnumSet, toList)
import GHC.Driver.Flags (GeneralFlag (..))
import GHC.Driver.Session (DynFlags (..), GhcNameVersion, PlatformMisc)
import GHC.Tc.Errors.Hole.FitTypes
    ( CandPlugin (..)
    , FitPlugin (..)
    , HoleFitPlugin (..)
    , TypedHole (..)
    )
import GHC.Tc.Types.Constraint
    ( CtLoc
    , Cts
    , Hole (..)
    , HoleSort (..)
    , Implication
    )
import GHC.Tc.Types.Evidence
    ( HoleExprRef (..)
    , HsWrapper
    , QuoteWrapper
    , TcEvBinds
    )
import GHC.Types.Basic
    ( Boxity
    , FunctionOrData (IsData, IsFunction)
    , Origin
    , PromotionFlag (..)
    , RecFlag
    )
import GHC.Types.Literal (LitNumType, Literal (..), pprLiteral)
import GHC.Types.Name
    ( HasOccName (occName)
    , Name (..)
    , NamedThing (getName)
    , OccName
    , getOccString
    , getSrcLoc
    , isDataConName
    , isHoleName
    , isSystemName
    , isTyConName
    , isTyVarName
    , nameStableString
    , nameUnique
    , pprDefinedAt
    , pprOccName
    )
import GHC.Types.Tickish (CoreTickish)
import GHC.Types.TyThing (TyThing (..))
import GHC.Types.Unique.Set (UniqSet, pprUniqSet)
import GHC.Types.Var
    ( Specificity (..)
    , TyCoVar
    , ForAllTyFlag (..)
    , FunTyFlag (..)
    , Var (..)
    , VarBndr (..)
    )
import GHC.Unit.Module.Warnings (WarningTxt, Warnings (..), pprWarningTxtForMsg)
import GHC.Unit.Types (GenModule (..), Unit (..))
import GHC.Utils.Encoding (utf8DecodeByteString)
import GHC.Utils.Outputable (Outputable (ppr), showSDocUnsafe)

import Data.List (intercalate, intersperse)

-- == for hsSyn ==

import Data.Generics.Uniplate.Data (Biplate)
import Data.Void (Void)
import GHC.Data.Bag (Bag (..), bagToList, isEmptyBag, unitBag)

-- ===============

-- * Show Instance are declared in the order of the data type declaration

-- | a haskell project is a list of core binds, type CoreProgram = [CoreBind], type CoreBind = Bind CoreBndr
deriving stock instance (Show bndr) => Show (Expr bndr)

-- * Deriving Show instance for Var Id
instance Show Var where
    show :: Var -> String
    show x = showSDocUnsafe $ ppr (varName x) -- show uniques aswell
    -- show = getOccString  -- show occurence name

-- * Deriving Show instance for Lit
instance Show Literal where
    show :: Literal -> String
    show (LitString l) = utf8DecodeByteString l
    show (LitChar c) = [c]
    show (LitNumber _ i) = show i
    show l = "lit"

-- * Deriving Show instance for Let
deriving stock instance (Show bndr) => Show (Bind bndr)

-- * Deriving Show instance for Case
deriving stock instance (Show bndr) => Show (Alt bndr)
deriving stock instance Show AltCon
instance Show DataCon where
    -- show d = "DCon: " ++ showSDocUnsafe (ppr d)  -- show both name and unique
    show :: DataCon -> String
    show d = showSDocUnsafe (ppr d)

-- name2Str (dataConName d)

-- * Deriving Show instance for Type
deriving stock instance Show Type

instance Show TyCon where
    show :: TyCon -> String
    show t = getOccString $ tyConName t

deriving stock instance Show Specificity

deriving stock instance Show ForAllTyFlag

deriving stock instance Show FunTyFlag

deriving stock instance (Show a, Show b) => Show (VarBndr a b)

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

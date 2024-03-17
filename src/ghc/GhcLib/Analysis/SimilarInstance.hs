{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use isJust" #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternGuards #-}

module GhcLib.Analysis.SimilarInstance where 

import GHC.Core ( Expr(..), CoreExpr, CoreBndr, Bind(..), CoreProgram, Alt(..), AltCon(..), CoreBind)
import GHC.Unit.Types
import GHC.Types.Var (Var(..), isTyVar, tyVarKind, isTcTyVar, isId)
import GHC.Core.TyCo.Rep (Type(..), Kind, TyLit (StrTyLit), AnonArgFlag (VisArg), Coercion)
import GHC.Core.Type (eqType)
import GHC.Types.Name (getOccString, occNameString)
import GHC.Data.FastString (fsLit)

import Debug.Trace ( trace )
import GhcLib.GHCRelated.ShowCore
import GHC.Core.TyCon (TyCon)
import Control.Monad (when)
import Data.Maybe (isNothing)
import GHC.Core.DataCon (DataCon(..), dataConName)
import GHC.Utils.Outputable (showSDocUnsafe, Outputable (ppr))
import GHC.Types.Literal (Literal(..), LitNumType)

import GhcLib.Transform.Utility
    ( isCaseExpr,
      isHoleExpr,
      isPatError,
      isHoleVar,
      isHoleVarExpr,
      getAltExp,
      isTrModuleVar )
import GHC.Core.Coercion (eqCoercion)
import GHC.Core.Utils (exprType)
import Data.Generics.Uniplate.Data (rewriteBi, Biplate)
import GHC.Core.Map.Type qualified as GHC 


class Similar a where
    (~=) ::  a -> a -> Bool
    (~>)  :: a -> a -> Bool

instance Similar CoreProgram where
    (x:xs) ~> (y:ys) = x ~> y && xs ~> ys
    [] ~> []         = True
    _ ~> _           = False -- trace "different number of binders" False

    (x:xs) ~= (y:ys) = x ~= y && xs ~= ys
    [] ~= []         = True
    _ ~= _           = False -- trace "different number of binders" False


--- Without trace -------------------------------------
instance Similar (Bind Var) where
    (~>) :: Bind Var -> Bind Var -> Bool
    (Rec es) ~> (Rec es')          = es ~> es' 
    (NonRec v e) ~> (NonRec v' e') | isTrModuleVar v, isTrModuleVar v' = True -- disregard module info binders 
                                   | otherwise = v ~> v' && e ~> e'
    (NonRec v e) ~> Rec ((v',e'):_) = v ~> v' && e ~> e' -- ! Not sure i'm a fan of this 
    _ ~> _                          = False

    (~=) :: Bind Var -> Bind Var -> Bool
    (Rec es) ~= (Rec es')          = es ~= es'
    (NonRec v e) ~= (NonRec v' e') | isTrModuleVar v, isTrModuleVar v' = True -- disregard module info binders 
                                   | otherwise = v ~= v' && e ~= e'
    x ~= y = False

instance Similar [(Var,Expr Var)] where
    (~>) :: [(Var, Expr Var)] -> [(Var, Expr Var)] -> Bool
    es ~> es' = all (\((b,e),(b',e')) -> b ~> b' && e ~> e') (zip es es') 

    (~=) :: [(Var, Expr Var)] -> [(Var, Expr Var)] -> Bool
    es ~= es' = all (\((b,e),(b',e')) -> b ~= b' && e ~= e') (zip es es') && length es == length es' 

instance Similar (Expr Var) where
    (~>) :: Expr Var -> Expr Var -> Bool
    (Var id) ~> (Var id')                   = id ~> id'   
    (Type t) ~> (Type t')                   = t ~> t'
    (Lit l)  ~> (Lit l')                    = l ~> l'
    (App (App f e) a) ~> (App (App f' e') a') | isCommutative f
                                                -- ! Why we want to check a function is commutative in case of a double application
                                                -- ! a, because it needs two arguments.

                                              , f ~> f' = (e ~> e' && a ~> a') || e ~> a' && e' ~> a
    (App e arg) ~> (App e' arg')            = e ~> e' && arg ~> arg'
    (Lam b e) ~> (Lam b' e')                = b  ~> b' && e ~> e' 
    (Case e v t as) ~> (Case e' v' t' as')  = e ~> e' && v ~> v' && as ~> as' -- && t  ~> t' 
    e            ~> (Case e' v t as)        = any ((e ~>) . getAltExp) as -- ! good point, student solution might be a partial solution
    (Cast e co)  ~> (Cast e' co')           = co ~> co' && e ~> e'
    (Let b e)    ~> (Let b' e')             = b  ~> b' && e ~> e'
    e            ~> (Let (Rec es) ine)      = any ((e ~>) . snd) es -- ! a partial solution
    (Coercion c) ~> (Coercion c')           = c  ~> c'
    (Tick _ e)   ~> (Tick _ e')             = e ~> e' 
    (Tick _ e)   ~> e'                      = e ~> e' 
    e            ~> (Tick _ e')             = e ~> e' 
    x ~> y                                  = isHoleVarExpr x || isHoleExpr x 

    (~=) :: Expr Var -> Expr Var -> Bool
    (Var id) ~= (Var id')                  = id ~= id'
    (Type t) ~= (Type t')                  = t ~= t'
    (Lit l)  ~= (Lit l')                   = l ~= l'
    (App (App f e) a) ~= (App (App f' e') a') | isCommutative f
                                               , f ~= f' = (e ~= e' && a ~= a') || e ~= a' && e' ~= a
    (App e arg) ~= (App e' arg')           = e ~= e' && arg ~= arg'

    (Lam b e) ~= (Lam b' e')               = b  ~= b' && e ~= e' -- check that the type of the head is equal                                      
    (Case e v t as) ~= (Case e' v' t' as') = e ~= e' && t  ~= t' && v ~= v' && as ~= as'
    (Cast e co)  ~= (Cast e' co')          = co ~= co' && e ~= e'
    (Let b e)    ~= (Let b' e')            = b  ~= b' && e ~= e'
    (Coercion c) ~= (Coercion c')          = c  ~= c'
    (Tick _ e)   ~= e'                     = e ~= e' 
    e            ~= (Tick _ e')            = e ~= e' 
    x ~= y                                 = False



instance Similar Coercion where
    c1 ~> c2 = True -- eqCoercion c1 c2 -- uniques of type vars might be a problem?  
    c1 ~= c2 = True


instance Similar Literal where
  (~>) :: Literal -> Literal -> Bool
  (LitString l)    ~> (LitString l')      = l == l' 
  (LitChar c)      ~> (LitChar c')        = c == c'
  (LitNumber ti i) ~> (LitNumber tj j)    = ti == tj && i == j
  (LitFloat f)     ~> (LitFloat f')       = f == f'
  (LitDouble d)    ~> (LitDouble d')      = d == d'
  (LitLabel _ _ fd) ~> (LitLabel _ _ fd') = fd == fd' 
  LitNullAddr      ~> LitNullAddr         = True 
  l ~> k                                  = False 

  (~=) :: Literal -> Literal -> Bool
  (~=) = (~>)

instance Similar [Alt Var] where
    []     ~> _  = True 
    (x:xs) ~> ys = matchUnordered x ys && xs ~> ys   
        where matchUnordered x []     = False 
              matchUnordered x (y:ys) | x ~> y    = True  
                                      | otherwise = matchUnordered x ys 
              
    xs ~= ys = all (uncurry (~=)) (zip xs ys) 
               && length xs == length ys 

instance Similar (Alt Var) where
    a@(Alt ac vs e) ~> (Alt ac' vs' e') 
            | isPatError e , not (isPatError e') = compareAlt -- if pattern error, student has missing cases, we dont check nested cases
            | not (isCaseExpr e) , isCaseExpr e', not (e ~> e') 
                                                = compareAlt && matchWithAlt a e' -- if comparing nested cases in model, need stricter check on expression
            | otherwise = compareAlt && e ~> e' 
        where compareAlt = ac ~> ac' && vs ~> vs'
              matchWithAlt (Alt ac _ e) (Case e' _ _ as) = any (\(Alt ac' _ e') -> ac ~> ac' && e ~> e') as 
    (Alt ac vs e) ~= (Alt ac' vs' e')  = ac ~= ac' && vs ~= vs' && e ~= e'

instance Similar [Var] where
    xs ~> ys = all (uncurry (~>)) (zip xs ys)
    xs ~= ys = length xs == length ys &&
        all (uncurry (~>)) (zip xs ys)

instance Similar AltCon where
    (DataAlt a) ~> (DataAlt a') = a ~> a'
    (LitAlt l)  ~> (LitAlt l')  = l ~> l'
    DEFAULT     ~> DEFAULT      = True
    _           ~> _            = False
    (~=) = (~>)

instance Similar DataCon where
    x ~> y = dcName x == dcName y
        where dcName = getOccString . dataConName  
    (~=) = (~>)

instance Similar Var where
    v1 ~> v2 = isHoleVar v1 || getOccString v1 == getOccString v2
    v1 ~= v2 = getOccString v1 == getOccString v2

instance Similar Type where
    k1 ~> k2 = GHC.deBruijnize k1 == GHC.deBruijnize k2  -- ! Could do some subtyping checking here 
    (~=) = (~>)
            -- to disregard uniques of typevars from different programs we don't use eqType
            -- using eqType would require same uniques, which we don't have across different compilations
            -- would require "renaming" all uniques from a large storage of fixed uniques 


{- we need checks for e.g  xs == reverse xs ~== reverse xs == xs 
however, pattern matching like below does not scale well at all. 
Need a clever approach. 
(App f@(App op e1) e2) ~== (App f'@(App op' e1') e2') 
                                                | isCommutative op =  trace ("op1" ++ show op ++ " op2:" ++ show op') $ 
                                                                op ~== op' && e1 ~== e1' || e2 ~== e2' || e1 ~== e2' && e2 ~== e1' 
                                                | otherwise = f ~== f' && e2 ~== e2' 
-}
isCommutative :: CoreExpr -> Bool
-- | Check if we are applying something commutative, then the order of the arguments are irrelevant
-- not sure how to check this, functions are just variables
isCommutative (Var op) = getOccString op `elem` ["==", "/=", "+", "*", "&&", "||"] -- hardcode common ones 
isCommutative _ = False


{-  (Lam b e) ~> e'                         | isAppToHole e = match e e'
                                        where match e e' = case e of 
                                                (Tick _ e) -> match e e' 
                                                (Lam v e)  -> match e e' 
                                                (App f args) | isHoleVarExpr args -> f ~> e' 
                                                             | otherwise          -> match args e' 
                                                ex -> trace ("GERE:" ++ show ex ++ "L") False  -}
r :: BiplateFor a => a -> a 
r = rewriteBi remTick 
    where remTick :: CoreExpr -> Maybe CoreExpr 
          remTick ex = case ex of 
            (Tick _ e) -> Just e 
            e -> Nothing 

class ( Biplate b CoreProgram, Biplate b CoreBind, Biplate b CoreExpr) 
      => BiplateFor b
instance BiplateFor CoreProgram      where
instance BiplateFor CoreBind         where
instance BiplateFor CoreExpr         where


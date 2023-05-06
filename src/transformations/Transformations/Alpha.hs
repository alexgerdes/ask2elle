

{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}

{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS -fno-warn-unused-top-binds -fno-warn-unused-matches #-}
-- UUAGC 0.9.52.2 (src/Language/Haskell/Transformations/Alpha.ag)
module Transformations.Alpha(alphaRename, AlphaInfo(..), alphaInfo, runTests) where

import Data.Generics.Uniplate.Direct 
import Data.List
import Data.Maybe
import qualified Data.Map as M
import Language.Haskell.Generated.Syntax
import Language.Haskell.Utility.Utils (noShadow)
import Test.QuickCheck


data AlphaInfo = AlphaInfo
    { free     :: Names   -- ^ Free variables in a Module
    , invSubst :: Subst   -- ^ Mapping from alpha name to original name
    , result   :: Module  -- ^ Alpha renamed Module
    } deriving (Show)

type Subst = M.Map Name Name

updateSubst :: Names -> Names -> Subst -> Subst
updateSubst ks vs subst = M.fromList (zip ks vs) `M.union` subst

substitute :: Name -> Subst -> Name
substitute k = fromMaybe k . M.lookup k

inverseSubst :: Subst -> Subst
inverseSubst = M.foldrWithKey (flip M.insert) M.empty

alphaInfo :: Names -> Module -> AlphaInfo
alphaInfo fixed = unc4 AlphaInfo . flip sem_Module fixed
  where 
    unc4 f (a, b, c) = f b c a

alphaRename :: Names -> Module -> Module
alphaRename fixed = result . alphaInfo fixed

freeVars :: Module -> Names
freeVars = free . alphaInfo []

fresh :: Names
fresh = [Ident ('x' : show i) | i <- [(1 :: Int)..]]

-- Test functions/values ------------------------------------------------------
f1, g, h, x1, y :: Name
f1 = Ident "f"; g = Ident "g"; h = Ident "h"; x1 = Ident "x"; y = Ident "y"

e :: Expr
e = Lambda [PVar x1] (Lambda [PVar x1] (Var x1))


d1, d2, d3 :: Decl
d1 = DPatBind (PVar f1) (Rhs e [d2])
d2 = DPatBind (PVar g) (Rhs e [])
d3 = DFunBinds [FunBind Nothing h [PVar x1, PVar y] (Rhs (Var (Ident "z") `App` [Var x1]) [])]

m1, m2, m3 :: Module
m1 = Module NoName $ Body [d1, d2, d3]

m2 = Module NoName (Body [DFunBinds [FunBind Nothing (Ident "f") [] (Rhs (Hole "1") [DHole "1"])]])

m3 = Module NoName (Body [DFunBinds [FunBind Nothing (Ident "allpairs") [PVar (Ident "xs")] (Rhs (App (Var (Ident "concatMap")) [Hole "6",Var (Ident "xs")]) [DFunBinds []])]])

-- Properties -----------------------------------------------------------------
propIdempotency :: Module -> Property 
propIdempotency m = let f = alphaRename [] in property $ f m == f (f m)

propUniqueBindVars :: Module -> Property
propUniqueBindVars m = let bs = bindings $ alphaRename [] m
                       in  property $ bs == nub bs

bindings :: Module -> Names
bindings m = [n | PVar n          <- universeBi m]
          ++ [n | PAs n _         <- universeBi m]
          ++ [f | FunBind _ f _ _ <- universeBi m]

propNoShadowing :: Module -> Property
propNoShadowing = property . noShadow . alphaRename []

propNoCapturing :: Module -> Property
propNoCapturing m = property $ freeVars m == freeVars (alphaRename [] m)

propFixedNames :: Module -> Property
propFixedNames m = forAll (genSlice $ names m) $ \fixed -> 
    null $ fixed \\ names (alphaRename fixed m)
  where
    names x = nub [n | n@(Ident _) <- universeBi x]

genSlice :: [a] -> Gen [a]
genSlice xs = do 
  i <- choose (0, length xs - 1)
  j <- choose (0, length xs - i)
  return $ take j $ drop i xs

propInverseSubst :: Module -> Property
propInverseSubst m = property $ transformBi (flip substitute subst) m' == m
  where
    AlphaInfo {invSubst = subst, result = m'} = alphaInfo [] m

runTests :: IO ()
runTests = quickCheck $ conjoin $ map ($ m1) [ propIdempotency, propNoShadowing
                                             , propNoCapturing, propFixedNames
                                             , propInverseSubst, propUniqueBindVars ]
{-# LINE 343 "src/Language/Haskell/Transformations/Alpha.hs" #-}
-- Alt ---------------------------------------------------------
-- cata
sem_Alt :: Alt ->
           T_Alt
sem_Alt (AHole _id) =
    (sem_Alt_AHole _id)
sem_Alt (Alt _feedback _pat _rhs) =
    (sem_Alt_Alt _feedback (sem_Pat _pat) (sem_Rhs _rhs))
sem_Alt (AltEmpty) =
    (sem_Alt_AltEmpty)
-- semantic domain
type T_Alt = Names ->
             Names ->
             Names ->
             Subst ->
             ( Alt,Names,Names,Subst)
sem_Alt_AHole :: HoleID ->
                 T_Alt
sem_Alt_AHole id_ =
    (\ _lhsIfixed
       _lhsIfresh
       _lhsIinscope
       _lhsIsubst ->
         (let _lhsOfree :: Names
              _lhsOinvSubst :: Subst
              _lhsOcopy :: Alt
              _lhsOfresh :: Names
              _lhsOfree =
                  ({-# LINE 103 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   []
                   {-# LINE 374 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOinvSubst =
                  ({-# LINE 104 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   M.empty
                   {-# LINE 379 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _copy =
                  ({-# LINE 105 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   AHole id_
                   {-# LINE 384 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 105 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _copy
                   {-# LINE 389 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 102 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfresh
                   {-# LINE 394 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
          in  ( _lhsOcopy,_lhsOfree,_lhsOfresh,_lhsOinvSubst)))
sem_Alt_Alt :: (Maybe String) ->
               T_Pat ->
               T_Rhs ->
               T_Alt
sem_Alt_Alt feedback_ pat_ rhs_ =
    (\ _lhsIfixed
       _lhsIfresh
       _lhsIinscope
       _lhsIsubst ->
         (let _rhsOinscope :: Names
              _rhsOsubst :: Subst
              _rhsOfresh :: Names
              _patOsubst :: Subst
              _lhsOfree :: Names
              _lhsOinvSubst :: Subst
              _lhsOcopy :: Alt
              _lhsOfresh :: Names
              _patOfixed :: Names
              _rhsOfixed :: Names
              _patIcopy :: Pat
              _patIinscope :: Names
              _patIinvSubst :: Subst
              _rhsIcopy :: Rhs
              _rhsIfree :: Names
              _rhsIfresh :: Names
              _rhsIinvSubst :: Subst
              _rhsOinscope =
                  ({-# LINE 154 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _inscope `union` _lhsIinscope
                   {-# LINE 426 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _rhsOsubst =
                  ({-# LINE 155 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _subst
                   {-# LINE 431 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _rhsOfresh =
                  ({-# LINE 156 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   drop (length _inscope) _lhsIfresh
                   {-# LINE 436 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _patOsubst =
                  ({-# LINE 157 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _subst
                   {-# LINE 441 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOfree =
                  ({-# LINE 158 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _rhsIfree \\ _patIinscope
                   {-# LINE 446 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _subst =
                  ({-# LINE 159 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   updateSubst _inscope _lhsIfresh _lhsIsubst
                   {-# LINE 451 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _inscope =
                  ({-# LINE 160 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _patIinscope \\ _lhsIfixed
                   {-# LINE 456 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOinvSubst =
                  ({-# LINE 104 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   (M.union _patIinvSubst _rhsIinvSubst)
                   {-# LINE 461 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _copy =
                  ({-# LINE 105 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   Alt feedback_ _patIcopy _rhsIcopy
                   {-# LINE 466 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 105 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _copy
                   {-# LINE 471 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 102 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _rhsIfresh
                   {-# LINE 476 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _patOfixed =
                  ({-# LINE 93 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfixed
                   {-# LINE 481 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _rhsOfixed =
                  ({-# LINE 101 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfixed
                   {-# LINE 486 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              ( _patIcopy,_patIinscope,_patIinvSubst) =
                  pat_ _patOfixed _patOsubst
              ( _rhsIcopy,_rhsIfree,_rhsIfresh,_rhsIinvSubst) =
                  rhs_ _rhsOfixed _rhsOfresh _rhsOinscope _rhsOsubst
          in  ( _lhsOcopy,_lhsOfree,_lhsOfresh,_lhsOinvSubst)))
sem_Alt_AltEmpty :: T_Alt
sem_Alt_AltEmpty =
    (\ _lhsIfixed
       _lhsIfresh
       _lhsIinscope
       _lhsIsubst ->
         (let _lhsOfree :: Names
              _lhsOinvSubst :: Subst
              _lhsOcopy :: Alt
              _lhsOfresh :: Names
              _lhsOfree =
                  ({-# LINE 103 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   []
                   {-# LINE 506 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOinvSubst =
                  ({-# LINE 104 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   M.empty
                   {-# LINE 511 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _copy =
                  ({-# LINE 105 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   AltEmpty
                   {-# LINE 516 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 105 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _copy
                   {-# LINE 521 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 102 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfresh
                   {-# LINE 526 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
          in  ( _lhsOcopy,_lhsOfree,_lhsOfresh,_lhsOinvSubst)))
-- Alts --------------------------------------------------------
-- cata
sem_Alts :: Alts ->
            T_Alts
sem_Alts list =
    (Prelude.foldr sem_Alts_Cons sem_Alts_Nil (Prelude.map sem_Alt list))
-- semantic domain
type T_Alts = Names ->
              Names ->
              Names ->
              Subst ->
              ( Alts,Names,Names,Subst)
sem_Alts_Cons :: T_Alt ->
                 T_Alts ->
                 T_Alts
sem_Alts_Cons hd_ tl_ =
    (\ _lhsIfixed
       _lhsIfresh
       _lhsIinscope
       _lhsIsubst ->
         (let _lhsOfree :: Names
              _lhsOinvSubst :: Subst
              _lhsOcopy :: Alts
              _lhsOfresh :: Names
              _hdOfixed :: Names
              _hdOfresh :: Names
              _hdOinscope :: Names
              _hdOsubst :: Subst
              _tlOfixed :: Names
              _tlOfresh :: Names
              _tlOinscope :: Names
              _tlOsubst :: Subst
              _hdIcopy :: Alt
              _hdIfree :: Names
              _hdIfresh :: Names
              _hdIinvSubst :: Subst
              _tlIcopy :: Alts
              _tlIfree :: Names
              _tlIfresh :: Names
              _tlIinvSubst :: Subst
              _lhsOfree =
                  ({-# LINE 103 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   (union _hdIfree _tlIfree)
                   {-# LINE 572 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOinvSubst =
                  ({-# LINE 104 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   (M.union _hdIinvSubst _tlIinvSubst)
                   {-# LINE 577 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _copy =
                  ({-# LINE 105 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   (:) _hdIcopy _tlIcopy
                   {-# LINE 582 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 105 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _copy
                   {-# LINE 587 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 102 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _tlIfresh
                   {-# LINE 592 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _hdOfixed =
                  ({-# LINE 101 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfixed
                   {-# LINE 597 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _hdOfresh =
                  ({-# LINE 102 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfresh
                   {-# LINE 602 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _hdOinscope =
                  ({-# LINE 99 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIinscope
                   {-# LINE 607 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _hdOsubst =
                  ({-# LINE 100 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIsubst
                   {-# LINE 612 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _tlOfixed =
                  ({-# LINE 101 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfixed
                   {-# LINE 617 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _tlOfresh =
                  ({-# LINE 102 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _hdIfresh
                   {-# LINE 622 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _tlOinscope =
                  ({-# LINE 99 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIinscope
                   {-# LINE 627 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _tlOsubst =
                  ({-# LINE 100 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIsubst
                   {-# LINE 632 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              ( _hdIcopy,_hdIfree,_hdIfresh,_hdIinvSubst) =
                  hd_ _hdOfixed _hdOfresh _hdOinscope _hdOsubst
              ( _tlIcopy,_tlIfree,_tlIfresh,_tlIinvSubst) =
                  tl_ _tlOfixed _tlOfresh _tlOinscope _tlOsubst
          in  ( _lhsOcopy,_lhsOfree,_lhsOfresh,_lhsOinvSubst)))
sem_Alts_Nil :: T_Alts
sem_Alts_Nil =
    (\ _lhsIfixed
       _lhsIfresh
       _lhsIinscope
       _lhsIsubst ->
         (let _lhsOfree :: Names
              _lhsOinvSubst :: Subst
              _lhsOcopy :: Alts
              _lhsOfresh :: Names
              _lhsOfree =
                  ({-# LINE 103 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   []
                   {-# LINE 652 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOinvSubst =
                  ({-# LINE 104 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   M.empty
                   {-# LINE 657 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _copy =
                  ({-# LINE 105 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   []
                   {-# LINE 662 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 105 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _copy
                   {-# LINE 667 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 102 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfresh
                   {-# LINE 672 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
          in  ( _lhsOcopy,_lhsOfree,_lhsOfresh,_lhsOinvSubst)))
-- Body --------------------------------------------------------
-- cata
sem_Body :: Body ->
            T_Body
sem_Body (BHole) =
    (sem_Body_BHole)
sem_Body (Body _decls) =
    (sem_Body_Body (sem_Decls _decls))
-- semantic domain
type T_Body = Names ->
              Names ->
              Names ->
              Subst ->
              ( Body,Names,Names,Names,Subst)
sem_Body_BHole :: T_Body
sem_Body_BHole =
    (\ _lhsIfixed
       _lhsIfresh
       _lhsIinscope
       _lhsIsubst ->
         (let _lhsOfree :: Names
              _lhsOinvSubst :: Subst
              _lhsOcopy :: Body
              _lhsOfresh :: Names
              _lhsOinscope :: Names
              _lhsOfree =
                  ({-# LINE 87 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   []
                   {-# LINE 703 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOinvSubst =
                  ({-# LINE 88 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   M.empty
                   {-# LINE 708 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _copy =
                  ({-# LINE 89 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   BHole
                   {-# LINE 713 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 89 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _copy
                   {-# LINE 718 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 86 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfresh
                   {-# LINE 723 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOinscope =
                  ({-# LINE 85 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIinscope
                   {-# LINE 728 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
          in  ( _lhsOcopy,_lhsOfree,_lhsOfresh,_lhsOinscope,_lhsOinvSubst)))
sem_Body_Body :: T_Decls ->
                 T_Body
sem_Body_Body decls_ =
    (\ _lhsIfixed
       _lhsIfresh
       _lhsIinscope
       _lhsIsubst ->
         (let _declsOinscope :: Names
              _declsOsubst :: Subst
              _declsOfresh :: Names
              _lhsOfree :: Names
              _lhsOinvSubst :: Subst
              _lhsOcopy :: Body
              _lhsOfresh :: Names
              _lhsOinscope :: Names
              _declsOfixed :: Names
              _declsIcopy :: Decls
              _declsIfree :: Names
              _declsIfresh :: Names
              _declsIinscope :: Names
              _declsIinvSubst :: Subst
              _declsOinscope =
                  ({-# LINE 116 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _inscope `union` _lhsIinscope
                   {-# LINE 755 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _declsOsubst =
                  ({-# LINE 117 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _subst
                   {-# LINE 760 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _declsOfresh =
                  ({-# LINE 118 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   drop (length _inscope) _lhsIfresh
                   {-# LINE 765 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOfree =
                  ({-# LINE 119 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _declsIfree \\ _declsIinscope
                   {-# LINE 770 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _subst =
                  ({-# LINE 120 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   updateSubst _inscope _lhsIfresh _lhsIsubst
                   {-# LINE 775 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _inscope =
                  ({-# LINE 121 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _declsIinscope \\ _lhsIfixed
                   {-# LINE 780 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOinvSubst =
                  ({-# LINE 88 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _declsIinvSubst
                   {-# LINE 785 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _copy =
                  ({-# LINE 89 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   Body _declsIcopy
                   {-# LINE 790 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 89 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _copy
                   {-# LINE 795 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 86 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _declsIfresh
                   {-# LINE 800 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOinscope =
                  ({-# LINE 85 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _inscope
                   {-# LINE 805 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _declsOfixed =
                  ({-# LINE 83 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfixed
                   {-# LINE 810 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              ( _declsIcopy,_declsIfree,_declsIfresh,_declsIinscope,_declsIinvSubst) =
                  decls_ _declsOfixed _declsOfresh _declsOinscope _declsOsubst
          in  ( _lhsOcopy,_lhsOfree,_lhsOfresh,_lhsOinscope,_lhsOinvSubst)))
-- Decl --------------------------------------------------------
-- cata
sem_Decl :: Decl ->
            T_Decl
sem_Decl (DHole _id) =
    (sem_Decl_DHole _id)
sem_Decl (DEmpty) =
    (sem_Decl_DEmpty)
sem_Decl (DFunBinds _funbinds) =
    (sem_Decl_DFunBinds (sem_FunBinds _funbinds))
sem_Decl (DPatBind _pat _rhs) =
    (sem_Decl_DPatBind (sem_Pat _pat) (sem_Rhs _rhs))
-- semantic domain
type T_Decl = Names ->
              Names ->
              Names ->
              Subst ->
              ( Decl,Names,Names,Names,Subst)
sem_Decl_DHole :: HoleID ->
                  T_Decl
sem_Decl_DHole id_ =
    (\ _lhsIfixed
       _lhsIfresh
       _lhsIinscope
       _lhsIsubst ->
         (let _lhsOinscope :: Names
              _lhsOfree :: Names
              _lhsOinvSubst :: Subst
              _lhsOcopy :: Decl
              _lhsOfresh :: Names
              _lhsOinscope =
                  ({-# LINE 126 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   []
                   {-# LINE 848 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOfree =
                  ({-# LINE 87 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   []
                   {-# LINE 853 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOinvSubst =
                  ({-# LINE 88 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   M.empty
                   {-# LINE 858 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _copy =
                  ({-# LINE 89 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   DHole id_
                   {-# LINE 863 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 89 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _copy
                   {-# LINE 868 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 86 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfresh
                   {-# LINE 873 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
          in  ( _lhsOcopy,_lhsOfree,_lhsOfresh,_lhsOinscope,_lhsOinvSubst)))
sem_Decl_DEmpty :: T_Decl
sem_Decl_DEmpty =
    (\ _lhsIfixed
       _lhsIfresh
       _lhsIinscope
       _lhsIsubst ->
         (let _lhsOfree :: Names
              _lhsOinvSubst :: Subst
              _lhsOcopy :: Decl
              _lhsOfresh :: Names
              _lhsOinscope :: Names
              _lhsOfree =
                  ({-# LINE 87 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   []
                   {-# LINE 890 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOinvSubst =
                  ({-# LINE 88 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   M.empty
                   {-# LINE 895 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _copy =
                  ({-# LINE 89 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   DEmpty
                   {-# LINE 900 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 89 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _copy
                   {-# LINE 905 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 86 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfresh
                   {-# LINE 910 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOinscope =
                  ({-# LINE 85 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIinscope
                   {-# LINE 915 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
          in  ( _lhsOcopy,_lhsOfree,_lhsOfresh,_lhsOinscope,_lhsOinvSubst)))
sem_Decl_DFunBinds :: T_FunBinds ->
                      T_Decl
sem_Decl_DFunBinds funbinds_ =
    (\ _lhsIfixed
       _lhsIfresh
       _lhsIinscope
       _lhsIsubst ->
         (let _lhsOinscope :: Names
              _lhsOfree :: Names
              _lhsOinvSubst :: Subst
              _lhsOcopy :: Decl
              _lhsOfresh :: Names
              _funbindsOfixed :: Names
              _funbindsOfresh :: Names
              _funbindsOinscope :: Names
              _funbindsOsubst :: Subst
              _funbindsIcopy :: FunBinds
              _funbindsIfree :: Names
              _funbindsIfresh :: Names
              _funbindsIinscope :: Names
              _funbindsIinvSubst :: Subst
              _lhsOinscope =
                  ({-# LINE 124 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   take 1 _funbindsIinscope
                   {-# LINE 942 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOfree =
                  ({-# LINE 87 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _funbindsIfree
                   {-# LINE 947 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOinvSubst =
                  ({-# LINE 88 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _funbindsIinvSubst
                   {-# LINE 952 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _copy =
                  ({-# LINE 89 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   DFunBinds _funbindsIcopy
                   {-# LINE 957 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 89 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _copy
                   {-# LINE 962 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 86 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _funbindsIfresh
                   {-# LINE 967 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _funbindsOfixed =
                  ({-# LINE 83 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfixed
                   {-# LINE 972 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _funbindsOfresh =
                  ({-# LINE 86 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfresh
                   {-# LINE 977 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _funbindsOinscope =
                  ({-# LINE 85 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIinscope
                   {-# LINE 982 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _funbindsOsubst =
                  ({-# LINE 82 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIsubst
                   {-# LINE 987 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              ( _funbindsIcopy,_funbindsIfree,_funbindsIfresh,_funbindsIinscope,_funbindsIinvSubst) =
                  funbinds_ _funbindsOfixed _funbindsOfresh _funbindsOinscope _funbindsOsubst
          in  ( _lhsOcopy,_lhsOfree,_lhsOfresh,_lhsOinscope,_lhsOinvSubst)))
sem_Decl_DPatBind :: T_Pat ->
                     T_Rhs ->
                     T_Decl
sem_Decl_DPatBind pat_ rhs_ =
    (\ _lhsIfixed
       _lhsIfresh
       _lhsIinscope
       _lhsIsubst ->
         (let _lhsOinscope :: Names
              _lhsOfree :: Names
              _lhsOinvSubst :: Subst
              _lhsOcopy :: Decl
              _lhsOfresh :: Names
              _patOfixed :: Names
              _patOsubst :: Subst
              _rhsOfixed :: Names
              _rhsOfresh :: Names
              _rhsOinscope :: Names
              _rhsOsubst :: Subst
              _patIcopy :: Pat
              _patIinscope :: Names
              _patIinvSubst :: Subst
              _rhsIcopy :: Rhs
              _rhsIfree :: Names
              _rhsIfresh :: Names
              _rhsIinvSubst :: Subst
              _lhsOinscope =
                  ({-# LINE 125 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _patIinscope
                   {-# LINE 1021 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOfree =
                  ({-# LINE 87 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _rhsIfree
                   {-# LINE 1026 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOinvSubst =
                  ({-# LINE 88 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   (M.union _patIinvSubst _rhsIinvSubst)
                   {-# LINE 1031 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _copy =
                  ({-# LINE 89 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   DPatBind _patIcopy _rhsIcopy
                   {-# LINE 1036 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 89 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _copy
                   {-# LINE 1041 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 86 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _rhsIfresh
                   {-# LINE 1046 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _patOfixed =
                  ({-# LINE 93 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfixed
                   {-# LINE 1051 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _patOsubst =
                  ({-# LINE 92 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIsubst
                   {-# LINE 1056 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _rhsOfixed =
                  ({-# LINE 101 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfixed
                   {-# LINE 1061 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _rhsOfresh =
                  ({-# LINE 102 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfresh
                   {-# LINE 1066 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _rhsOinscope =
                  ({-# LINE 99 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _patIinscope
                   {-# LINE 1071 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _rhsOsubst =
                  ({-# LINE 100 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIsubst
                   {-# LINE 1076 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              ( _patIcopy,_patIinscope,_patIinvSubst) =
                  pat_ _patOfixed _patOsubst
              ( _rhsIcopy,_rhsIfree,_rhsIfresh,_rhsIinvSubst) =
                  rhs_ _rhsOfixed _rhsOfresh _rhsOinscope _rhsOsubst
          in  ( _lhsOcopy,_lhsOfree,_lhsOfresh,_lhsOinscope,_lhsOinvSubst)))
-- Decls -------------------------------------------------------
-- cata
sem_Decls :: Decls ->
             T_Decls
sem_Decls list =
    (Prelude.foldr sem_Decls_Cons sem_Decls_Nil (Prelude.map sem_Decl list))
-- semantic domain
type T_Decls = Names ->
               Names ->
               Names ->
               Subst ->
               ( Decls,Names,Names,Names,Subst)
sem_Decls_Cons :: T_Decl ->
                  T_Decls ->
                  T_Decls
sem_Decls_Cons hd_ tl_ =
    (\ _lhsIfixed
       _lhsIfresh
       _lhsIinscope
       _lhsIsubst ->
         (let _lhsOinscope :: Names
              _lhsOfree :: Names
              _lhsOinvSubst :: Subst
              _lhsOcopy :: Decls
              _lhsOfresh :: Names
              _hdOfixed :: Names
              _hdOfresh :: Names
              _hdOinscope :: Names
              _hdOsubst :: Subst
              _tlOfixed :: Names
              _tlOfresh :: Names
              _tlOinscope :: Names
              _tlOsubst :: Subst
              _hdIcopy :: Decl
              _hdIfree :: Names
              _hdIfresh :: Names
              _hdIinscope :: Names
              _hdIinvSubst :: Subst
              _tlIcopy :: Decls
              _tlIfree :: Names
              _tlIfresh :: Names
              _tlIinscope :: Names
              _tlIinvSubst :: Subst
              _lhsOinscope =
                  ({-# LINE 130 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _hdIinscope `union` _tlIinscope
                   {-# LINE 1129 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOfree =
                  ({-# LINE 87 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   (union _hdIfree _tlIfree)
                   {-# LINE 1134 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOinvSubst =
                  ({-# LINE 88 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   (M.union _hdIinvSubst _tlIinvSubst)
                   {-# LINE 1139 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _copy =
                  ({-# LINE 89 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   (:) _hdIcopy _tlIcopy
                   {-# LINE 1144 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 89 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _copy
                   {-# LINE 1149 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 86 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _tlIfresh
                   {-# LINE 1154 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _hdOfixed =
                  ({-# LINE 83 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfixed
                   {-# LINE 1159 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _hdOfresh =
                  ({-# LINE 86 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfresh
                   {-# LINE 1164 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _hdOinscope =
                  ({-# LINE 85 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIinscope
                   {-# LINE 1169 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _hdOsubst =
                  ({-# LINE 82 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIsubst
                   {-# LINE 1174 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _tlOfixed =
                  ({-# LINE 83 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfixed
                   {-# LINE 1179 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _tlOfresh =
                  ({-# LINE 86 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _hdIfresh
                   {-# LINE 1184 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _tlOinscope =
                  ({-# LINE 85 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _hdIinscope
                   {-# LINE 1189 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _tlOsubst =
                  ({-# LINE 82 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIsubst
                   {-# LINE 1194 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              ( _hdIcopy,_hdIfree,_hdIfresh,_hdIinscope,_hdIinvSubst) =
                  hd_ _hdOfixed _hdOfresh _hdOinscope _hdOsubst
              ( _tlIcopy,_tlIfree,_tlIfresh,_tlIinscope,_tlIinvSubst) =
                  tl_ _tlOfixed _tlOfresh _tlOinscope _tlOsubst
          in  ( _lhsOcopy,_lhsOfree,_lhsOfresh,_lhsOinscope,_lhsOinvSubst)))
sem_Decls_Nil :: T_Decls
sem_Decls_Nil =
    (\ _lhsIfixed
       _lhsIfresh
       _lhsIinscope
       _lhsIsubst ->
         (let _lhsOinscope :: Names
              _lhsOfree :: Names
              _lhsOinvSubst :: Subst
              _lhsOcopy :: Decls
              _lhsOfresh :: Names
              _lhsOinscope =
                  ({-# LINE 129 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   []
                   {-# LINE 1215 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOfree =
                  ({-# LINE 87 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   []
                   {-# LINE 1220 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOinvSubst =
                  ({-# LINE 88 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   M.empty
                   {-# LINE 1225 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _copy =
                  ({-# LINE 89 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   []
                   {-# LINE 1230 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 89 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _copy
                   {-# LINE 1235 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 86 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfresh
                   {-# LINE 1240 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
          in  ( _lhsOcopy,_lhsOfree,_lhsOfresh,_lhsOinscope,_lhsOinvSubst)))
-- Expr --------------------------------------------------------
-- cata
sem_Expr :: Expr ->
            T_Expr
sem_Expr (Hole _id) =
    (sem_Expr_Hole _id)
sem_Expr (Feedback _feedback _expr) =
    (sem_Expr_Feedback _feedback (sem_Expr _expr))
sem_Expr (MustUse _expr) =
    (sem_Expr_MustUse (sem_Expr _expr))
sem_Expr (Eta _n _expr) =
    (sem_Expr_Eta _n (sem_Expr _expr))
sem_Expr (Refactor _expr _alts) =
    (sem_Expr_Refactor (sem_Expr _expr) (sem_RefactorChoices _alts))
sem_Expr (Case _expr _alts) =
    (sem_Expr_Case (sem_Expr _expr) (sem_Alts _alts))
sem_Expr (Con _name) =
    (sem_Expr_Con (sem_Name _name))
sem_Expr (If _cond _then _else) =
    (sem_Expr_If (sem_Expr _cond) (sem_Expr _then) (sem_Expr _else))
sem_Expr (InfixApp _left _op _right) =
    (sem_Expr_InfixApp (sem_MaybeExpr _left) (sem_Expr _op) (sem_MaybeExpr _right))
sem_Expr (Lambda _pats _expr) =
    (sem_Expr_Lambda (sem_Pats _pats) (sem_Expr _expr))
sem_Expr (Let _decls _expr) =
    (sem_Expr_Let (sem_Decls _decls) (sem_Expr _expr))
sem_Expr (Lit _lit) =
    (sem_Expr_Lit (sem_Literal _lit))
sem_Expr (App _fun _args) =
    (sem_Expr_App (sem_Expr _fun) (sem_Exprs _args))
sem_Expr (Paren _expr) =
    (sem_Expr_Paren (sem_Expr _expr))
sem_Expr (Tuple _exprs) =
    (sem_Expr_Tuple (sem_Exprs _exprs))
sem_Expr (Var _name) =
    (sem_Expr_Var (sem_Name _name))
sem_Expr (Enum _from _then _to) =
    (sem_Expr_Enum (sem_Expr _from) (sem_MaybeExpr _then) (sem_MaybeExpr _to))
sem_Expr (List _exprs) =
    (sem_Expr_List (sem_Exprs _exprs))
sem_Expr (Neg _expr) =
    (sem_Expr_Neg (sem_Expr _expr))
-- semantic domain
type T_Expr = Names ->
              Names ->
              Names ->
              Subst ->
              ( Expr,Names,Names,Subst)
sem_Expr_Hole :: HoleID ->
                 T_Expr
sem_Expr_Hole id_ =
    (\ _lhsIfixed
       _lhsIfresh
       _lhsIinscope
       _lhsIsubst ->
         (let _lhsOfree :: Names
              _lhsOinvSubst :: Subst
              _lhsOcopy :: Expr
              _lhsOfresh :: Names
              _lhsOfree =
                  ({-# LINE 103 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   []
                   {-# LINE 1305 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOinvSubst =
                  ({-# LINE 104 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   M.empty
                   {-# LINE 1310 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _copy =
                  ({-# LINE 105 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   Hole id_
                   {-# LINE 1315 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 105 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _copy
                   {-# LINE 1320 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 102 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfresh
                   {-# LINE 1325 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
          in  ( _lhsOcopy,_lhsOfree,_lhsOfresh,_lhsOinvSubst)))
sem_Expr_Feedback :: String ->
                     T_Expr ->
                     T_Expr
sem_Expr_Feedback feedback_ expr_ =
    (\ _lhsIfixed
       _lhsIfresh
       _lhsIinscope
       _lhsIsubst ->
         (let _lhsOfree :: Names
              _lhsOinvSubst :: Subst
              _lhsOcopy :: Expr
              _lhsOfresh :: Names
              _exprOfixed :: Names
              _exprOfresh :: Names
              _exprOinscope :: Names
              _exprOsubst :: Subst
              _exprIcopy :: Expr
              _exprIfree :: Names
              _exprIfresh :: Names
              _exprIinvSubst :: Subst
              _lhsOfree =
                  ({-# LINE 103 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _exprIfree
                   {-# LINE 1351 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOinvSubst =
                  ({-# LINE 104 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _exprIinvSubst
                   {-# LINE 1356 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _copy =
                  ({-# LINE 105 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   Feedback feedback_ _exprIcopy
                   {-# LINE 1361 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 105 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _copy
                   {-# LINE 1366 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 102 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _exprIfresh
                   {-# LINE 1371 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _exprOfixed =
                  ({-# LINE 101 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfixed
                   {-# LINE 1376 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _exprOfresh =
                  ({-# LINE 102 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfresh
                   {-# LINE 1381 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _exprOinscope =
                  ({-# LINE 99 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIinscope
                   {-# LINE 1386 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _exprOsubst =
                  ({-# LINE 100 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIsubst
                   {-# LINE 1391 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              ( _exprIcopy,_exprIfree,_exprIfresh,_exprIinvSubst) =
                  expr_ _exprOfixed _exprOfresh _exprOinscope _exprOsubst
          in  ( _lhsOcopy,_lhsOfree,_lhsOfresh,_lhsOinvSubst)))
sem_Expr_MustUse :: T_Expr ->
                    T_Expr
sem_Expr_MustUse expr_ =
    (\ _lhsIfixed
       _lhsIfresh
       _lhsIinscope
       _lhsIsubst ->
         (let _lhsOfree :: Names
              _lhsOinvSubst :: Subst
              _lhsOcopy :: Expr
              _lhsOfresh :: Names
              _exprOfixed :: Names
              _exprOfresh :: Names
              _exprOinscope :: Names
              _exprOsubst :: Subst
              _exprIcopy :: Expr
              _exprIfree :: Names
              _exprIfresh :: Names
              _exprIinvSubst :: Subst
              _lhsOfree =
                  ({-# LINE 103 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _exprIfree
                   {-# LINE 1418 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOinvSubst =
                  ({-# LINE 104 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _exprIinvSubst
                   {-# LINE 1423 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _copy =
                  ({-# LINE 105 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   MustUse _exprIcopy
                   {-# LINE 1428 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 105 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _copy
                   {-# LINE 1433 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 102 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _exprIfresh
                   {-# LINE 1438 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _exprOfixed =
                  ({-# LINE 101 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfixed
                   {-# LINE 1443 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _exprOfresh =
                  ({-# LINE 102 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfresh
                   {-# LINE 1448 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _exprOinscope =
                  ({-# LINE 99 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIinscope
                   {-# LINE 1453 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _exprOsubst =
                  ({-# LINE 100 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIsubst
                   {-# LINE 1458 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              ( _exprIcopy,_exprIfree,_exprIfresh,_exprIinvSubst) =
                  expr_ _exprOfixed _exprOfresh _exprOinscope _exprOsubst
          in  ( _lhsOcopy,_lhsOfree,_lhsOfresh,_lhsOinvSubst)))
sem_Expr_Eta :: Int ->
                T_Expr ->
                T_Expr
sem_Expr_Eta n_ expr_ =
    (\ _lhsIfixed
       _lhsIfresh
       _lhsIinscope
       _lhsIsubst ->
         (let _lhsOfree :: Names
              _lhsOinvSubst :: Subst
              _lhsOcopy :: Expr
              _lhsOfresh :: Names
              _exprOfixed :: Names
              _exprOfresh :: Names
              _exprOinscope :: Names
              _exprOsubst :: Subst
              _exprIcopy :: Expr
              _exprIfree :: Names
              _exprIfresh :: Names
              _exprIinvSubst :: Subst
              _lhsOfree =
                  ({-# LINE 103 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _exprIfree
                   {-# LINE 1486 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOinvSubst =
                  ({-# LINE 104 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _exprIinvSubst
                   {-# LINE 1491 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _copy =
                  ({-# LINE 105 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   Eta n_ _exprIcopy
                   {-# LINE 1496 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 105 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _copy
                   {-# LINE 1501 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 102 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _exprIfresh
                   {-# LINE 1506 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _exprOfixed =
                  ({-# LINE 101 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfixed
                   {-# LINE 1511 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _exprOfresh =
                  ({-# LINE 102 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfresh
                   {-# LINE 1516 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _exprOinscope =
                  ({-# LINE 99 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIinscope
                   {-# LINE 1521 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _exprOsubst =
                  ({-# LINE 100 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIsubst
                   {-# LINE 1526 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              ( _exprIcopy,_exprIfree,_exprIfresh,_exprIinvSubst) =
                  expr_ _exprOfixed _exprOfresh _exprOinscope _exprOsubst
          in  ( _lhsOcopy,_lhsOfree,_lhsOfresh,_lhsOinvSubst)))
sem_Expr_Refactor :: T_Expr ->
                     T_RefactorChoices ->
                     T_Expr
sem_Expr_Refactor expr_ alts_ =
    (\ _lhsIfixed
       _lhsIfresh
       _lhsIinscope
       _lhsIsubst ->
         (let _lhsOfree :: Names
              _lhsOinvSubst :: Subst
              _lhsOcopy :: Expr
              _lhsOfresh :: Names
              _exprOfixed :: Names
              _exprOfresh :: Names
              _exprOinscope :: Names
              _exprOsubst :: Subst
              _altsOfixed :: Names
              _altsOfresh :: Names
              _altsOinscope :: Names
              _altsOsubst :: Subst
              _exprIcopy :: Expr
              _exprIfree :: Names
              _exprIfresh :: Names
              _exprIinvSubst :: Subst
              _altsIcopy :: RefactorChoices
              _altsIfree :: Names
              _altsIfresh :: Names
              _altsIinvSubst :: Subst
              _lhsOfree =
                  ({-# LINE 103 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   (union _exprIfree _altsIfree)
                   {-# LINE 1562 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOinvSubst =
                  ({-# LINE 104 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   (M.union _exprIinvSubst _altsIinvSubst)
                   {-# LINE 1567 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _copy =
                  ({-# LINE 105 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   Refactor _exprIcopy _altsIcopy
                   {-# LINE 1572 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 105 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _copy
                   {-# LINE 1577 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 102 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _altsIfresh
                   {-# LINE 1582 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _exprOfixed =
                  ({-# LINE 101 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfixed
                   {-# LINE 1587 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _exprOfresh =
                  ({-# LINE 102 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfresh
                   {-# LINE 1592 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _exprOinscope =
                  ({-# LINE 99 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIinscope
                   {-# LINE 1597 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _exprOsubst =
                  ({-# LINE 100 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIsubst
                   {-# LINE 1602 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _altsOfixed =
                  ({-# LINE 101 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfixed
                   {-# LINE 1607 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _altsOfresh =
                  ({-# LINE 102 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _exprIfresh
                   {-# LINE 1612 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _altsOinscope =
                  ({-# LINE 99 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIinscope
                   {-# LINE 1617 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _altsOsubst =
                  ({-# LINE 100 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIsubst
                   {-# LINE 1622 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              ( _exprIcopy,_exprIfree,_exprIfresh,_exprIinvSubst) =
                  expr_ _exprOfixed _exprOfresh _exprOinscope _exprOsubst
              ( _altsIcopy,_altsIfree,_altsIfresh,_altsIinvSubst) =
                  alts_ _altsOfixed _altsOfresh _altsOinscope _altsOsubst
          in  ( _lhsOcopy,_lhsOfree,_lhsOfresh,_lhsOinvSubst)))
sem_Expr_Case :: T_Expr ->
                 T_Alts ->
                 T_Expr
sem_Expr_Case expr_ alts_ =
    (\ _lhsIfixed
       _lhsIfresh
       _lhsIinscope
       _lhsIsubst ->
         (let _lhsOfree :: Names
              _lhsOinvSubst :: Subst
              _lhsOcopy :: Expr
              _lhsOfresh :: Names
              _exprOfixed :: Names
              _exprOfresh :: Names
              _exprOinscope :: Names
              _exprOsubst :: Subst
              _altsOfixed :: Names
              _altsOfresh :: Names
              _altsOinscope :: Names
              _altsOsubst :: Subst
              _exprIcopy :: Expr
              _exprIfree :: Names
              _exprIfresh :: Names
              _exprIinvSubst :: Subst
              _altsIcopy :: Alts
              _altsIfree :: Names
              _altsIfresh :: Names
              _altsIinvSubst :: Subst
              _lhsOfree =
                  ({-# LINE 103 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   (union _exprIfree _altsIfree)
                   {-# LINE 1660 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOinvSubst =
                  ({-# LINE 104 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   (M.union _exprIinvSubst _altsIinvSubst)
                   {-# LINE 1665 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _copy =
                  ({-# LINE 105 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   Case _exprIcopy _altsIcopy
                   {-# LINE 1670 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 105 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _copy
                   {-# LINE 1675 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 102 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _altsIfresh
                   {-# LINE 1680 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _exprOfixed =
                  ({-# LINE 101 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfixed
                   {-# LINE 1685 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _exprOfresh =
                  ({-# LINE 102 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfresh
                   {-# LINE 1690 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _exprOinscope =
                  ({-# LINE 99 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIinscope
                   {-# LINE 1695 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _exprOsubst =
                  ({-# LINE 100 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIsubst
                   {-# LINE 1700 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _altsOfixed =
                  ({-# LINE 101 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfixed
                   {-# LINE 1705 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _altsOfresh =
                  ({-# LINE 102 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _exprIfresh
                   {-# LINE 1710 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _altsOinscope =
                  ({-# LINE 99 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIinscope
                   {-# LINE 1715 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _altsOsubst =
                  ({-# LINE 100 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIsubst
                   {-# LINE 1720 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              ( _exprIcopy,_exprIfree,_exprIfresh,_exprIinvSubst) =
                  expr_ _exprOfixed _exprOfresh _exprOinscope _exprOsubst
              ( _altsIcopy,_altsIfree,_altsIfresh,_altsIinvSubst) =
                  alts_ _altsOfixed _altsOfresh _altsOinscope _altsOsubst
          in  ( _lhsOcopy,_lhsOfree,_lhsOfresh,_lhsOinvSubst)))
sem_Expr_Con :: T_Name ->
                T_Expr
sem_Expr_Con name_ =
    (\ _lhsIfixed
       _lhsIfresh
       _lhsIinscope
       _lhsIsubst ->
         (let _lhsOfree :: Names
              _lhsOinvSubst :: Subst
              _lhsOcopy :: Expr
              _lhsOfresh :: Names
              _nameIcopy :: Name
              _lhsOfree =
                  ({-# LINE 103 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   []
                   {-# LINE 1742 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOinvSubst =
                  ({-# LINE 104 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   M.empty
                   {-# LINE 1747 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _copy =
                  ({-# LINE 105 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   Con _nameIcopy
                   {-# LINE 1752 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 105 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _copy
                   {-# LINE 1757 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 102 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfresh
                   {-# LINE 1762 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              ( _nameIcopy) =
                  name_
          in  ( _lhsOcopy,_lhsOfree,_lhsOfresh,_lhsOinvSubst)))
sem_Expr_If :: T_Expr ->
               T_Expr ->
               T_Expr ->
               T_Expr
sem_Expr_If cond_ then_ else_ =
    (\ _lhsIfixed
       _lhsIfresh
       _lhsIinscope
       _lhsIsubst ->
         (let _lhsOfree :: Names
              _lhsOinvSubst :: Subst
              _lhsOcopy :: Expr
              _lhsOfresh :: Names
              _condOfixed :: Names
              _condOfresh :: Names
              _condOinscope :: Names
              _condOsubst :: Subst
              _thenOfixed :: Names
              _thenOfresh :: Names
              _thenOinscope :: Names
              _thenOsubst :: Subst
              _elseOfixed :: Names
              _elseOfresh :: Names
              _elseOinscope :: Names
              _elseOsubst :: Subst
              _condIcopy :: Expr
              _condIfree :: Names
              _condIfresh :: Names
              _condIinvSubst :: Subst
              _thenIcopy :: Expr
              _thenIfree :: Names
              _thenIfresh :: Names
              _thenIinvSubst :: Subst
              _elseIcopy :: Expr
              _elseIfree :: Names
              _elseIfresh :: Names
              _elseIinvSubst :: Subst
              _lhsOfree =
                  ({-# LINE 103 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   (union _condIfree (union _thenIfree _elseIfree))
                   {-# LINE 1807 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOinvSubst =
                  ({-# LINE 104 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   (M.union _condIinvSubst (M.union _thenIinvSubst _elseIinvSubst))
                   {-# LINE 1812 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _copy =
                  ({-# LINE 105 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   If _condIcopy _thenIcopy _elseIcopy
                   {-# LINE 1817 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 105 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _copy
                   {-# LINE 1822 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 102 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _elseIfresh
                   {-# LINE 1827 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _condOfixed =
                  ({-# LINE 101 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfixed
                   {-# LINE 1832 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _condOfresh =
                  ({-# LINE 102 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfresh
                   {-# LINE 1837 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _condOinscope =
                  ({-# LINE 99 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIinscope
                   {-# LINE 1842 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _condOsubst =
                  ({-# LINE 100 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIsubst
                   {-# LINE 1847 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _thenOfixed =
                  ({-# LINE 101 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfixed
                   {-# LINE 1852 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _thenOfresh =
                  ({-# LINE 102 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _condIfresh
                   {-# LINE 1857 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _thenOinscope =
                  ({-# LINE 99 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIinscope
                   {-# LINE 1862 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _thenOsubst =
                  ({-# LINE 100 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIsubst
                   {-# LINE 1867 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _elseOfixed =
                  ({-# LINE 101 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfixed
                   {-# LINE 1872 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _elseOfresh =
                  ({-# LINE 102 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _thenIfresh
                   {-# LINE 1877 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _elseOinscope =
                  ({-# LINE 99 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIinscope
                   {-# LINE 1882 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _elseOsubst =
                  ({-# LINE 100 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIsubst
                   {-# LINE 1887 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              ( _condIcopy,_condIfree,_condIfresh,_condIinvSubst) =
                  cond_ _condOfixed _condOfresh _condOinscope _condOsubst
              ( _thenIcopy,_thenIfree,_thenIfresh,_thenIinvSubst) =
                  then_ _thenOfixed _thenOfresh _thenOinscope _thenOsubst
              ( _elseIcopy,_elseIfree,_elseIfresh,_elseIinvSubst) =
                  else_ _elseOfixed _elseOfresh _elseOinscope _elseOsubst
          in  ( _lhsOcopy,_lhsOfree,_lhsOfresh,_lhsOinvSubst)))
sem_Expr_InfixApp :: T_MaybeExpr ->
                     T_Expr ->
                     T_MaybeExpr ->
                     T_Expr
sem_Expr_InfixApp left_ op_ right_ =
    (\ _lhsIfixed
       _lhsIfresh
       _lhsIinscope
       _lhsIsubst ->
         (let _lhsOfree :: Names
              _lhsOinvSubst :: Subst
              _lhsOcopy :: Expr
              _lhsOfresh :: Names
              _leftOfixed :: Names
              _leftOfresh :: Names
              _leftOinscope :: Names
              _leftOsubst :: Subst
              _opOfixed :: Names
              _opOfresh :: Names
              _opOinscope :: Names
              _opOsubst :: Subst
              _rightOfixed :: Names
              _rightOfresh :: Names
              _rightOinscope :: Names
              _rightOsubst :: Subst
              _leftIcopy :: MaybeExpr
              _leftIfree :: Names
              _leftIfresh :: Names
              _leftIinvSubst :: Subst
              _opIcopy :: Expr
              _opIfree :: Names
              _opIfresh :: Names
              _opIinvSubst :: Subst
              _rightIcopy :: MaybeExpr
              _rightIfree :: Names
              _rightIfresh :: Names
              _rightIinvSubst :: Subst
              _lhsOfree =
                  ({-# LINE 103 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   (union _leftIfree (union _opIfree _rightIfree))
                   {-# LINE 1936 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOinvSubst =
                  ({-# LINE 104 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   (M.union _leftIinvSubst (M.union _opIinvSubst _rightIinvSubst))
                   {-# LINE 1941 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _copy =
                  ({-# LINE 105 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   InfixApp _leftIcopy _opIcopy _rightIcopy
                   {-# LINE 1946 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 105 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _copy
                   {-# LINE 1951 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 102 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _rightIfresh
                   {-# LINE 1956 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _leftOfixed =
                  ({-# LINE 101 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfixed
                   {-# LINE 1961 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _leftOfresh =
                  ({-# LINE 102 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfresh
                   {-# LINE 1966 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _leftOinscope =
                  ({-# LINE 99 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIinscope
                   {-# LINE 1971 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _leftOsubst =
                  ({-# LINE 100 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIsubst
                   {-# LINE 1976 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _opOfixed =
                  ({-# LINE 101 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfixed
                   {-# LINE 1981 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _opOfresh =
                  ({-# LINE 102 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _leftIfresh
                   {-# LINE 1986 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _opOinscope =
                  ({-# LINE 99 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIinscope
                   {-# LINE 1991 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _opOsubst =
                  ({-# LINE 100 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIsubst
                   {-# LINE 1996 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _rightOfixed =
                  ({-# LINE 101 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfixed
                   {-# LINE 2001 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _rightOfresh =
                  ({-# LINE 102 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _opIfresh
                   {-# LINE 2006 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _rightOinscope =
                  ({-# LINE 99 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIinscope
                   {-# LINE 2011 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _rightOsubst =
                  ({-# LINE 100 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIsubst
                   {-# LINE 2016 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              ( _leftIcopy,_leftIfree,_leftIfresh,_leftIinvSubst) =
                  left_ _leftOfixed _leftOfresh _leftOinscope _leftOsubst
              ( _opIcopy,_opIfree,_opIfresh,_opIinvSubst) =
                  op_ _opOfixed _opOfresh _opOinscope _opOsubst
              ( _rightIcopy,_rightIfree,_rightIfresh,_rightIinvSubst) =
                  right_ _rightOfixed _rightOfresh _rightOinscope _rightOsubst
          in  ( _lhsOcopy,_lhsOfree,_lhsOfresh,_lhsOinvSubst)))
sem_Expr_Lambda :: T_Pats ->
                   T_Expr ->
                   T_Expr
sem_Expr_Lambda pats_ expr_ =
    (\ _lhsIfixed
       _lhsIfresh
       _lhsIinscope
       _lhsIsubst ->
         (let _exprOinscope :: Names
              _exprOsubst :: Subst
              _exprOfresh :: Names
              _patsOsubst :: Subst
              _lhsOfree :: Names
              _lhsOinvSubst :: Subst
              _lhsOcopy :: Expr
              _lhsOfresh :: Names
              _patsOfixed :: Names
              _exprOfixed :: Names
              _patsIcopy :: Pats
              _patsIinscope :: Names
              _patsIinvSubst :: Subst
              _exprIcopy :: Expr
              _exprIfree :: Names
              _exprIfresh :: Names
              _exprIinvSubst :: Subst
              _exprOinscope =
                  ({-# LINE 136 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _inscope `union` _lhsIinscope
                   {-# LINE 2053 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _exprOsubst =
                  ({-# LINE 137 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _subst
                   {-# LINE 2058 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _exprOfresh =
                  ({-# LINE 138 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   drop (length _inscope) _lhsIfresh
                   {-# LINE 2063 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _patsOsubst =
                  ({-# LINE 139 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _subst
                   {-# LINE 2068 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOfree =
                  ({-# LINE 140 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _exprIfree \\ _patsIinscope
                   {-# LINE 2073 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _subst =
                  ({-# LINE 141 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   updateSubst _inscope _lhsIfresh _lhsIsubst
                   {-# LINE 2078 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _inscope =
                  ({-# LINE 142 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _patsIinscope \\ _lhsIfixed
                   {-# LINE 2083 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOinvSubst =
                  ({-# LINE 104 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   (M.union _patsIinvSubst _exprIinvSubst)
                   {-# LINE 2088 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _copy =
                  ({-# LINE 105 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   Lambda _patsIcopy _exprIcopy
                   {-# LINE 2093 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 105 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _copy
                   {-# LINE 2098 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 102 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _exprIfresh
                   {-# LINE 2103 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _patsOfixed =
                  ({-# LINE 93 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfixed
                   {-# LINE 2108 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _exprOfixed =
                  ({-# LINE 101 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfixed
                   {-# LINE 2113 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              ( _patsIcopy,_patsIinscope,_patsIinvSubst) =
                  pats_ _patsOfixed _patsOsubst
              ( _exprIcopy,_exprIfree,_exprIfresh,_exprIinvSubst) =
                  expr_ _exprOfixed _exprOfresh _exprOinscope _exprOsubst
          in  ( _lhsOcopy,_lhsOfree,_lhsOfresh,_lhsOinvSubst)))
sem_Expr_Let :: T_Decls ->
                T_Expr ->
                T_Expr
sem_Expr_Let decls_ expr_ =
    (\ _lhsIfixed
       _lhsIfresh
       _lhsIinscope
       _lhsIsubst ->
         (let _exprOinscope :: Names
              _exprOsubst :: Subst
              _exprOfresh :: Names
              _declsOsubst :: Subst
              _declsOinscope :: Names
              _declsOfresh :: Names
              _lhsOfree :: Names
              _lhsOinvSubst :: Subst
              _lhsOcopy :: Expr
              _lhsOfresh :: Names
              _declsOfixed :: Names
              _exprOfixed :: Names
              _declsIcopy :: Decls
              _declsIfree :: Names
              _declsIfresh :: Names
              _declsIinscope :: Names
              _declsIinvSubst :: Subst
              _exprIcopy :: Expr
              _exprIfree :: Names
              _exprIfresh :: Names
              _exprIinvSubst :: Subst
              _exprOinscope =
                  ({-# LINE 143 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _inscope `union` _lhsIinscope
                   {-# LINE 2152 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _exprOsubst =
                  ({-# LINE 144 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _subst
                   {-# LINE 2157 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _exprOfresh =
                  ({-# LINE 145 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _declsIfresh
                   {-# LINE 2162 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _declsOsubst =
                  ({-# LINE 146 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _subst
                   {-# LINE 2167 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _declsOinscope =
                  ({-# LINE 147 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _inscope `union` _lhsIinscope
                   {-# LINE 2172 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _declsOfresh =
                  ({-# LINE 148 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   drop (length _inscope) _lhsIfresh
                   {-# LINE 2177 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOfree =
                  ({-# LINE 149 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _exprIfree \\ _declsIinscope
                   {-# LINE 2182 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _subst =
                  ({-# LINE 150 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   updateSubst _inscope _lhsIfresh _lhsIsubst
                   {-# LINE 2187 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _inscope =
                  ({-# LINE 151 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _declsIinscope \\ _lhsIfixed
                   {-# LINE 2192 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOinvSubst =
                  ({-# LINE 104 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   (M.union _declsIinvSubst _exprIinvSubst)
                   {-# LINE 2197 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _copy =
                  ({-# LINE 105 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   Let _declsIcopy _exprIcopy
                   {-# LINE 2202 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 105 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _copy
                   {-# LINE 2207 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 102 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _exprIfresh
                   {-# LINE 2212 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _declsOfixed =
                  ({-# LINE 83 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfixed
                   {-# LINE 2217 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _exprOfixed =
                  ({-# LINE 101 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfixed
                   {-# LINE 2222 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              ( _declsIcopy,_declsIfree,_declsIfresh,_declsIinscope,_declsIinvSubst) =
                  decls_ _declsOfixed _declsOfresh _declsOinscope _declsOsubst
              ( _exprIcopy,_exprIfree,_exprIfresh,_exprIinvSubst) =
                  expr_ _exprOfixed _exprOfresh _exprOinscope _exprOsubst
          in  ( _lhsOcopy,_lhsOfree,_lhsOfresh,_lhsOinvSubst)))
sem_Expr_Lit :: T_Literal ->
                T_Expr
sem_Expr_Lit lit_ =
    (\ _lhsIfixed
       _lhsIfresh
       _lhsIinscope
       _lhsIsubst ->
         (let _lhsOfree :: Names
              _lhsOinvSubst :: Subst
              _lhsOcopy :: Expr
              _lhsOfresh :: Names
              _litIcopy :: Literal
              _lhsOfree =
                  ({-# LINE 103 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   []
                   {-# LINE 2244 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOinvSubst =
                  ({-# LINE 104 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   M.empty
                   {-# LINE 2249 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _copy =
                  ({-# LINE 105 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   Lit _litIcopy
                   {-# LINE 2254 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 105 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _copy
                   {-# LINE 2259 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 102 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfresh
                   {-# LINE 2264 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              ( _litIcopy) =
                  lit_
          in  ( _lhsOcopy,_lhsOfree,_lhsOfresh,_lhsOinvSubst)))
sem_Expr_App :: T_Expr ->
                T_Exprs ->
                T_Expr
sem_Expr_App fun_ args_ =
    (\ _lhsIfixed
       _lhsIfresh
       _lhsIinscope
       _lhsIsubst ->
         (let _lhsOfree :: Names
              _lhsOinvSubst :: Subst
              _lhsOcopy :: Expr
              _lhsOfresh :: Names
              _funOfixed :: Names
              _funOfresh :: Names
              _funOinscope :: Names
              _funOsubst :: Subst
              _argsOfixed :: Names
              _argsOfresh :: Names
              _argsOinscope :: Names
              _argsOsubst :: Subst
              _funIcopy :: Expr
              _funIfree :: Names
              _funIfresh :: Names
              _funIinvSubst :: Subst
              _argsIcopy :: Exprs
              _argsIfree :: Names
              _argsIfresh :: Names
              _argsIinvSubst :: Subst
              _lhsOfree =
                  ({-# LINE 103 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   (union _funIfree _argsIfree)
                   {-# LINE 2300 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOinvSubst =
                  ({-# LINE 104 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   (M.union _funIinvSubst _argsIinvSubst)
                   {-# LINE 2305 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _copy =
                  ({-# LINE 105 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   App _funIcopy _argsIcopy
                   {-# LINE 2310 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 105 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _copy
                   {-# LINE 2315 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 102 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _argsIfresh
                   {-# LINE 2320 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _funOfixed =
                  ({-# LINE 101 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfixed
                   {-# LINE 2325 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _funOfresh =
                  ({-# LINE 102 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfresh
                   {-# LINE 2330 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _funOinscope =
                  ({-# LINE 99 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIinscope
                   {-# LINE 2335 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _funOsubst =
                  ({-# LINE 100 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIsubst
                   {-# LINE 2340 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _argsOfixed =
                  ({-# LINE 101 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfixed
                   {-# LINE 2345 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _argsOfresh =
                  ({-# LINE 102 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _funIfresh
                   {-# LINE 2350 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _argsOinscope =
                  ({-# LINE 99 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIinscope
                   {-# LINE 2355 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _argsOsubst =
                  ({-# LINE 100 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIsubst
                   {-# LINE 2360 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              ( _funIcopy,_funIfree,_funIfresh,_funIinvSubst) =
                  fun_ _funOfixed _funOfresh _funOinscope _funOsubst
              ( _argsIcopy,_argsIfree,_argsIfresh,_argsIinvSubst) =
                  args_ _argsOfixed _argsOfresh _argsOinscope _argsOsubst
          in  ( _lhsOcopy,_lhsOfree,_lhsOfresh,_lhsOinvSubst)))
sem_Expr_Paren :: T_Expr ->
                  T_Expr
sem_Expr_Paren expr_ =
    (\ _lhsIfixed
       _lhsIfresh
       _lhsIinscope
       _lhsIsubst ->
         (let _lhsOfree :: Names
              _lhsOinvSubst :: Subst
              _lhsOcopy :: Expr
              _lhsOfresh :: Names
              _exprOfixed :: Names
              _exprOfresh :: Names
              _exprOinscope :: Names
              _exprOsubst :: Subst
              _exprIcopy :: Expr
              _exprIfree :: Names
              _exprIfresh :: Names
              _exprIinvSubst :: Subst
              _lhsOfree =
                  ({-# LINE 103 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _exprIfree
                   {-# LINE 2389 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOinvSubst =
                  ({-# LINE 104 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _exprIinvSubst
                   {-# LINE 2394 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _copy =
                  ({-# LINE 105 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   Paren _exprIcopy
                   {-# LINE 2399 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 105 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _copy
                   {-# LINE 2404 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 102 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _exprIfresh
                   {-# LINE 2409 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _exprOfixed =
                  ({-# LINE 101 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfixed
                   {-# LINE 2414 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _exprOfresh =
                  ({-# LINE 102 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfresh
                   {-# LINE 2419 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _exprOinscope =
                  ({-# LINE 99 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIinscope
                   {-# LINE 2424 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _exprOsubst =
                  ({-# LINE 100 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIsubst
                   {-# LINE 2429 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              ( _exprIcopy,_exprIfree,_exprIfresh,_exprIinvSubst) =
                  expr_ _exprOfixed _exprOfresh _exprOinscope _exprOsubst
          in  ( _lhsOcopy,_lhsOfree,_lhsOfresh,_lhsOinvSubst)))
sem_Expr_Tuple :: T_Exprs ->
                  T_Expr
sem_Expr_Tuple exprs_ =
    (\ _lhsIfixed
       _lhsIfresh
       _lhsIinscope
       _lhsIsubst ->
         (let _lhsOfree :: Names
              _lhsOinvSubst :: Subst
              _lhsOcopy :: Expr
              _lhsOfresh :: Names
              _exprsOfixed :: Names
              _exprsOfresh :: Names
              _exprsOinscope :: Names
              _exprsOsubst :: Subst
              _exprsIcopy :: Exprs
              _exprsIfree :: Names
              _exprsIfresh :: Names
              _exprsIinvSubst :: Subst
              _lhsOfree =
                  ({-# LINE 103 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _exprsIfree
                   {-# LINE 2456 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOinvSubst =
                  ({-# LINE 104 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _exprsIinvSubst
                   {-# LINE 2461 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _copy =
                  ({-# LINE 105 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   Tuple _exprsIcopy
                   {-# LINE 2466 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 105 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _copy
                   {-# LINE 2471 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 102 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _exprsIfresh
                   {-# LINE 2476 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _exprsOfixed =
                  ({-# LINE 101 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfixed
                   {-# LINE 2481 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _exprsOfresh =
                  ({-# LINE 102 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfresh
                   {-# LINE 2486 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _exprsOinscope =
                  ({-# LINE 99 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIinscope
                   {-# LINE 2491 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _exprsOsubst =
                  ({-# LINE 100 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIsubst
                   {-# LINE 2496 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              ( _exprsIcopy,_exprsIfree,_exprsIfresh,_exprsIinvSubst) =
                  exprs_ _exprsOfixed _exprsOfresh _exprsOinscope _exprsOsubst
          in  ( _lhsOcopy,_lhsOfree,_lhsOfresh,_lhsOinvSubst)))
sem_Expr_Var :: T_Name ->
                T_Expr
sem_Expr_Var name_ =
    (\ _lhsIfixed
       _lhsIfresh
       _lhsIinscope
       _lhsIsubst ->
         (let _lhsOfree :: Names
              _lhsOcopy :: Expr
              _lhsOinvSubst :: Subst
              _lhsOfresh :: Names
              _nameIcopy :: Name
              _lhsOfree =
                  ({-# LINE 133 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   [_nameIcopy]
                   {-# LINE 2516 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 134 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   Var $ substitute _nameIcopy _lhsIsubst
                   {-# LINE 2521 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOinvSubst =
                  ({-# LINE 135 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   inverseSubst _lhsIsubst
                   {-# LINE 2526 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _copy =
                  ({-# LINE 105 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   Var _nameIcopy
                   {-# LINE 2531 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 102 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfresh
                   {-# LINE 2536 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              ( _nameIcopy) =
                  name_
          in  ( _lhsOcopy,_lhsOfree,_lhsOfresh,_lhsOinvSubst)))
sem_Expr_Enum :: T_Expr ->
                 T_MaybeExpr ->
                 T_MaybeExpr ->
                 T_Expr
sem_Expr_Enum from_ then_ to_ =
    (\ _lhsIfixed
       _lhsIfresh
       _lhsIinscope
       _lhsIsubst ->
         (let _lhsOfree :: Names
              _lhsOinvSubst :: Subst
              _lhsOcopy :: Expr
              _lhsOfresh :: Names
              _fromOfixed :: Names
              _fromOfresh :: Names
              _fromOinscope :: Names
              _fromOsubst :: Subst
              _thenOfixed :: Names
              _thenOfresh :: Names
              _thenOinscope :: Names
              _thenOsubst :: Subst
              _toOfixed :: Names
              _toOfresh :: Names
              _toOinscope :: Names
              _toOsubst :: Subst
              _fromIcopy :: Expr
              _fromIfree :: Names
              _fromIfresh :: Names
              _fromIinvSubst :: Subst
              _thenIcopy :: MaybeExpr
              _thenIfree :: Names
              _thenIfresh :: Names
              _thenIinvSubst :: Subst
              _toIcopy :: MaybeExpr
              _toIfree :: Names
              _toIfresh :: Names
              _toIinvSubst :: Subst
              _lhsOfree =
                  ({-# LINE 103 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   (union _fromIfree (union _thenIfree _toIfree))
                   {-# LINE 2581 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOinvSubst =
                  ({-# LINE 104 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   (M.union _fromIinvSubst (M.union _thenIinvSubst _toIinvSubst))
                   {-# LINE 2586 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _copy =
                  ({-# LINE 105 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   Enum _fromIcopy _thenIcopy _toIcopy
                   {-# LINE 2591 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 105 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _copy
                   {-# LINE 2596 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 102 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _toIfresh
                   {-# LINE 2601 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _fromOfixed =
                  ({-# LINE 101 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfixed
                   {-# LINE 2606 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _fromOfresh =
                  ({-# LINE 102 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfresh
                   {-# LINE 2611 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _fromOinscope =
                  ({-# LINE 99 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIinscope
                   {-# LINE 2616 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _fromOsubst =
                  ({-# LINE 100 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIsubst
                   {-# LINE 2621 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _thenOfixed =
                  ({-# LINE 101 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfixed
                   {-# LINE 2626 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _thenOfresh =
                  ({-# LINE 102 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _fromIfresh
                   {-# LINE 2631 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _thenOinscope =
                  ({-# LINE 99 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIinscope
                   {-# LINE 2636 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _thenOsubst =
                  ({-# LINE 100 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIsubst
                   {-# LINE 2641 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _toOfixed =
                  ({-# LINE 101 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfixed
                   {-# LINE 2646 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _toOfresh =
                  ({-# LINE 102 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _thenIfresh
                   {-# LINE 2651 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _toOinscope =
                  ({-# LINE 99 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIinscope
                   {-# LINE 2656 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _toOsubst =
                  ({-# LINE 100 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIsubst
                   {-# LINE 2661 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              ( _fromIcopy,_fromIfree,_fromIfresh,_fromIinvSubst) =
                  from_ _fromOfixed _fromOfresh _fromOinscope _fromOsubst
              ( _thenIcopy,_thenIfree,_thenIfresh,_thenIinvSubst) =
                  then_ _thenOfixed _thenOfresh _thenOinscope _thenOsubst
              ( _toIcopy,_toIfree,_toIfresh,_toIinvSubst) =
                  to_ _toOfixed _toOfresh _toOinscope _toOsubst
          in  ( _lhsOcopy,_lhsOfree,_lhsOfresh,_lhsOinvSubst)))
sem_Expr_List :: T_Exprs ->
                 T_Expr
sem_Expr_List exprs_ =
    (\ _lhsIfixed
       _lhsIfresh
       _lhsIinscope
       _lhsIsubst ->
         (let _lhsOfree :: Names
              _lhsOinvSubst :: Subst
              _lhsOcopy :: Expr
              _lhsOfresh :: Names
              _exprsOfixed :: Names
              _exprsOfresh :: Names
              _exprsOinscope :: Names
              _exprsOsubst :: Subst
              _exprsIcopy :: Exprs
              _exprsIfree :: Names
              _exprsIfresh :: Names
              _exprsIinvSubst :: Subst
              _lhsOfree =
                  ({-# LINE 103 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _exprsIfree
                   {-# LINE 2692 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOinvSubst =
                  ({-# LINE 104 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _exprsIinvSubst
                   {-# LINE 2697 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _copy =
                  ({-# LINE 105 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   List _exprsIcopy
                   {-# LINE 2702 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 105 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _copy
                   {-# LINE 2707 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 102 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _exprsIfresh
                   {-# LINE 2712 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _exprsOfixed =
                  ({-# LINE 101 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfixed
                   {-# LINE 2717 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _exprsOfresh =
                  ({-# LINE 102 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfresh
                   {-# LINE 2722 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _exprsOinscope =
                  ({-# LINE 99 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIinscope
                   {-# LINE 2727 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _exprsOsubst =
                  ({-# LINE 100 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIsubst
                   {-# LINE 2732 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              ( _exprsIcopy,_exprsIfree,_exprsIfresh,_exprsIinvSubst) =
                  exprs_ _exprsOfixed _exprsOfresh _exprsOinscope _exprsOsubst
          in  ( _lhsOcopy,_lhsOfree,_lhsOfresh,_lhsOinvSubst)))
sem_Expr_Neg :: T_Expr ->
                T_Expr
sem_Expr_Neg expr_ =
    (\ _lhsIfixed
       _lhsIfresh
       _lhsIinscope
       _lhsIsubst ->
         (let _lhsOfree :: Names
              _lhsOinvSubst :: Subst
              _lhsOcopy :: Expr
              _lhsOfresh :: Names
              _exprOfixed :: Names
              _exprOfresh :: Names
              _exprOinscope :: Names
              _exprOsubst :: Subst
              _exprIcopy :: Expr
              _exprIfree :: Names
              _exprIfresh :: Names
              _exprIinvSubst :: Subst
              _lhsOfree =
                  ({-# LINE 103 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _exprIfree
                   {-# LINE 2759 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOinvSubst =
                  ({-# LINE 104 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _exprIinvSubst
                   {-# LINE 2764 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _copy =
                  ({-# LINE 105 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   Neg _exprIcopy
                   {-# LINE 2769 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 105 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _copy
                   {-# LINE 2774 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 102 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _exprIfresh
                   {-# LINE 2779 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _exprOfixed =
                  ({-# LINE 101 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfixed
                   {-# LINE 2784 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _exprOfresh =
                  ({-# LINE 102 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfresh
                   {-# LINE 2789 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _exprOinscope =
                  ({-# LINE 99 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIinscope
                   {-# LINE 2794 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _exprOsubst =
                  ({-# LINE 100 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIsubst
                   {-# LINE 2799 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              ( _exprIcopy,_exprIfree,_exprIfresh,_exprIinvSubst) =
                  expr_ _exprOfixed _exprOfresh _exprOinscope _exprOsubst
          in  ( _lhsOcopy,_lhsOfree,_lhsOfresh,_lhsOinvSubst)))
-- Exprs -------------------------------------------------------
-- cata
sem_Exprs :: Exprs ->
             T_Exprs
sem_Exprs list =
    (Prelude.foldr sem_Exprs_Cons sem_Exprs_Nil (Prelude.map sem_Expr list))
-- semantic domain
type T_Exprs = Names ->
               Names ->
               Names ->
               Subst ->
               ( Exprs,Names,Names,Subst)
sem_Exprs_Cons :: T_Expr ->
                  T_Exprs ->
                  T_Exprs
sem_Exprs_Cons hd_ tl_ =
    (\ _lhsIfixed
       _lhsIfresh
       _lhsIinscope
       _lhsIsubst ->
         (let _lhsOfree :: Names
              _lhsOinvSubst :: Subst
              _lhsOcopy :: Exprs
              _lhsOfresh :: Names
              _hdOfixed :: Names
              _hdOfresh :: Names
              _hdOinscope :: Names
              _hdOsubst :: Subst
              _tlOfixed :: Names
              _tlOfresh :: Names
              _tlOinscope :: Names
              _tlOsubst :: Subst
              _hdIcopy :: Expr
              _hdIfree :: Names
              _hdIfresh :: Names
              _hdIinvSubst :: Subst
              _tlIcopy :: Exprs
              _tlIfree :: Names
              _tlIfresh :: Names
              _tlIinvSubst :: Subst
              _lhsOfree =
                  ({-# LINE 103 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   (union _hdIfree _tlIfree)
                   {-# LINE 2847 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOinvSubst =
                  ({-# LINE 104 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   (M.union _hdIinvSubst _tlIinvSubst)
                   {-# LINE 2852 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _copy =
                  ({-# LINE 105 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   (:) _hdIcopy _tlIcopy
                   {-# LINE 2857 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 105 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _copy
                   {-# LINE 2862 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 102 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _tlIfresh
                   {-# LINE 2867 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _hdOfixed =
                  ({-# LINE 101 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfixed
                   {-# LINE 2872 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _hdOfresh =
                  ({-# LINE 102 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfresh
                   {-# LINE 2877 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _hdOinscope =
                  ({-# LINE 99 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIinscope
                   {-# LINE 2882 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _hdOsubst =
                  ({-# LINE 100 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIsubst
                   {-# LINE 2887 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _tlOfixed =
                  ({-# LINE 101 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfixed
                   {-# LINE 2892 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _tlOfresh =
                  ({-# LINE 102 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _hdIfresh
                   {-# LINE 2897 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _tlOinscope =
                  ({-# LINE 99 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIinscope
                   {-# LINE 2902 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _tlOsubst =
                  ({-# LINE 100 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIsubst
                   {-# LINE 2907 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              ( _hdIcopy,_hdIfree,_hdIfresh,_hdIinvSubst) =
                  hd_ _hdOfixed _hdOfresh _hdOinscope _hdOsubst
              ( _tlIcopy,_tlIfree,_tlIfresh,_tlIinvSubst) =
                  tl_ _tlOfixed _tlOfresh _tlOinscope _tlOsubst
          in  ( _lhsOcopy,_lhsOfree,_lhsOfresh,_lhsOinvSubst)))
sem_Exprs_Nil :: T_Exprs
sem_Exprs_Nil =
    (\ _lhsIfixed
       _lhsIfresh
       _lhsIinscope
       _lhsIsubst ->
         (let _lhsOfree :: Names
              _lhsOinvSubst :: Subst
              _lhsOcopy :: Exprs
              _lhsOfresh :: Names
              _lhsOfree =
                  ({-# LINE 103 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   []
                   {-# LINE 2927 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOinvSubst =
                  ({-# LINE 104 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   M.empty
                   {-# LINE 2932 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _copy =
                  ({-# LINE 105 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   []
                   {-# LINE 2937 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 105 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _copy
                   {-# LINE 2942 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 102 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfresh
                   {-# LINE 2947 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
          in  ( _lhsOcopy,_lhsOfree,_lhsOfresh,_lhsOinvSubst)))
-- FunBind -----------------------------------------------------
-- cata
sem_FunBind :: FunBind ->
               T_FunBind
sem_FunBind (FBHole _id) =
    (sem_FunBind_FBHole _id)
sem_FunBind (FunBind _feedback _name _pats _rhs) =
    (sem_FunBind_FunBind _feedback (sem_Name _name) (sem_Pats _pats) (sem_Rhs _rhs))
-- semantic domain
type T_FunBind = Names ->
                 Names ->
                 Names ->
                 Subst ->
                 ( FunBind,Names,Names,Names,Subst)
sem_FunBind_FBHole :: HoleID ->
                      T_FunBind
sem_FunBind_FBHole id_ =
    (\ _lhsIfixed
       _lhsIfresh
       _lhsIinscope
       _lhsIsubst ->
         (let _lhsOinscope :: Names
              _lhsOfree :: Names
              _lhsOinvSubst :: Subst
              _lhsOcopy :: FunBind
              _lhsOfresh :: Names
              _lhsOinscope =
                  ({-# LINE 176 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   []
                   {-# LINE 2979 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOfree =
                  ({-# LINE 87 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   []
                   {-# LINE 2984 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOinvSubst =
                  ({-# LINE 88 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   M.empty
                   {-# LINE 2989 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _copy =
                  ({-# LINE 89 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   FBHole id_
                   {-# LINE 2994 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 89 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _copy
                   {-# LINE 2999 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 86 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfresh
                   {-# LINE 3004 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
          in  ( _lhsOcopy,_lhsOfree,_lhsOfresh,_lhsOinscope,_lhsOinvSubst)))
sem_FunBind_FunBind :: (Maybe String) ->
                       T_Name ->
                       T_Pats ->
                       T_Rhs ->
                       T_FunBind
sem_FunBind_FunBind feedback_ name_ pats_ rhs_ =
    (\ _lhsIfixed
       _lhsIfresh
       _lhsIinscope
       _lhsIsubst ->
         (let _rhsOinscope :: Names
              _rhsOsubst :: Subst
              _rhsOfresh :: Names
              _patsOsubst :: Subst
              _lhsOinscope :: Names
              _lhsOfree :: Names
              _lhsOcopy :: FunBind
              _lhsOinvSubst :: Subst
              _lhsOfresh :: Names
              _patsOfixed :: Names
              _rhsOfixed :: Names
              _nameIcopy :: Name
              _patsIcopy :: Pats
              _patsIinscope :: Names
              _patsIinvSubst :: Subst
              _rhsIcopy :: Rhs
              _rhsIfree :: Names
              _rhsIfresh :: Names
              _rhsIinvSubst :: Subst
              _rhsOinscope =
                  ({-# LINE 166 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _inscope `union` _lhsIinscope
                   {-# LINE 3039 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _rhsOsubst =
                  ({-# LINE 167 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _subst
                   {-# LINE 3044 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _rhsOfresh =
                  ({-# LINE 168 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   drop (length _inscope) _lhsIfresh
                   {-# LINE 3049 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _patsOsubst =
                  ({-# LINE 169 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _subst
                   {-# LINE 3054 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOinscope =
                  ({-# LINE 170 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   [_nameIcopy]
                   {-# LINE 3059 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOfree =
                  ({-# LINE 171 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _rhsIfree \\ _patsIinscope
                   {-# LINE 3064 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 172 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   FunBind feedback_ (substitute _nameIcopy _lhsIsubst) _patsIcopy _rhsIcopy
                   {-# LINE 3069 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOinvSubst =
                  ({-# LINE 173 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   inverseSubst _lhsIsubst `M.union` _patsIinvSubst
                   {-# LINE 3074 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _subst =
                  ({-# LINE 174 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   updateSubst _inscope _lhsIfresh _lhsIsubst
                   {-# LINE 3079 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _inscope =
                  ({-# LINE 175 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _patsIinscope \\ _lhsIfixed
                   {-# LINE 3084 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _copy =
                  ({-# LINE 89 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   FunBind feedback_ _nameIcopy _patsIcopy _rhsIcopy
                   {-# LINE 3089 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 86 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _rhsIfresh
                   {-# LINE 3094 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _patsOfixed =
                  ({-# LINE 93 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfixed
                   {-# LINE 3099 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _rhsOfixed =
                  ({-# LINE 101 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfixed
                   {-# LINE 3104 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              ( _nameIcopy) =
                  name_
              ( _patsIcopy,_patsIinscope,_patsIinvSubst) =
                  pats_ _patsOfixed _patsOsubst
              ( _rhsIcopy,_rhsIfree,_rhsIfresh,_rhsIinvSubst) =
                  rhs_ _rhsOfixed _rhsOfresh _rhsOinscope _rhsOsubst
          in  ( _lhsOcopy,_lhsOfree,_lhsOfresh,_lhsOinscope,_lhsOinvSubst)))
-- FunBinds ----------------------------------------------------
-- cata
sem_FunBinds :: FunBinds ->
                T_FunBinds
sem_FunBinds list =
    (Prelude.foldr sem_FunBinds_Cons sem_FunBinds_Nil (Prelude.map sem_FunBind list))
-- semantic domain
type T_FunBinds = Names ->
                  Names ->
                  Names ->
                  Subst ->
                  ( FunBinds,Names,Names,Names,Subst)
sem_FunBinds_Cons :: T_FunBind ->
                     T_FunBinds ->
                     T_FunBinds
sem_FunBinds_Cons hd_ tl_ =
    (\ _lhsIfixed
       _lhsIfresh
       _lhsIinscope
       _lhsIsubst ->
         (let _lhsOinscope :: Names
              _lhsOfree :: Names
              _lhsOinvSubst :: Subst
              _lhsOcopy :: FunBinds
              _lhsOfresh :: Names
              _hdOfixed :: Names
              _hdOfresh :: Names
              _hdOinscope :: Names
              _hdOsubst :: Subst
              _tlOfixed :: Names
              _tlOfresh :: Names
              _tlOinscope :: Names
              _tlOsubst :: Subst
              _hdIcopy :: FunBind
              _hdIfree :: Names
              _hdIfresh :: Names
              _hdIinscope :: Names
              _hdIinvSubst :: Subst
              _tlIcopy :: FunBinds
              _tlIfree :: Names
              _tlIfresh :: Names
              _tlIinscope :: Names
              _tlIinvSubst :: Subst
              _lhsOinscope =
                  ({-# LINE 163 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _hdIinscope `union` _tlIinscope
                   {-# LINE 3159 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOfree =
                  ({-# LINE 87 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   (union _hdIfree _tlIfree)
                   {-# LINE 3164 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOinvSubst =
                  ({-# LINE 88 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   (M.union _hdIinvSubst _tlIinvSubst)
                   {-# LINE 3169 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _copy =
                  ({-# LINE 89 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   (:) _hdIcopy _tlIcopy
                   {-# LINE 3174 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 89 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _copy
                   {-# LINE 3179 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 86 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _tlIfresh
                   {-# LINE 3184 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _hdOfixed =
                  ({-# LINE 83 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfixed
                   {-# LINE 3189 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _hdOfresh =
                  ({-# LINE 86 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfresh
                   {-# LINE 3194 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _hdOinscope =
                  ({-# LINE 85 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIinscope
                   {-# LINE 3199 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _hdOsubst =
                  ({-# LINE 82 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIsubst
                   {-# LINE 3204 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _tlOfixed =
                  ({-# LINE 83 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfixed
                   {-# LINE 3209 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _tlOfresh =
                  ({-# LINE 86 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _hdIfresh
                   {-# LINE 3214 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _tlOinscope =
                  ({-# LINE 85 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _hdIinscope
                   {-# LINE 3219 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _tlOsubst =
                  ({-# LINE 82 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIsubst
                   {-# LINE 3224 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              ( _hdIcopy,_hdIfree,_hdIfresh,_hdIinscope,_hdIinvSubst) =
                  hd_ _hdOfixed _hdOfresh _hdOinscope _hdOsubst
              ( _tlIcopy,_tlIfree,_tlIfresh,_tlIinscope,_tlIinvSubst) =
                  tl_ _tlOfixed _tlOfresh _tlOinscope _tlOsubst
          in  ( _lhsOcopy,_lhsOfree,_lhsOfresh,_lhsOinscope,_lhsOinvSubst)))
sem_FunBinds_Nil :: T_FunBinds
sem_FunBinds_Nil =
    (\ _lhsIfixed
       _lhsIfresh
       _lhsIinscope
       _lhsIsubst ->
         (let _lhsOinscope :: Names
              _lhsOfree :: Names
              _lhsOinvSubst :: Subst
              _lhsOcopy :: FunBinds
              _lhsOfresh :: Names
              _lhsOinscope =
                  ({-# LINE 162 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   []
                   {-# LINE 3245 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOfree =
                  ({-# LINE 87 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   []
                   {-# LINE 3250 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOinvSubst =
                  ({-# LINE 88 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   M.empty
                   {-# LINE 3255 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _copy =
                  ({-# LINE 89 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   []
                   {-# LINE 3260 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 89 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _copy
                   {-# LINE 3265 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 86 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfresh
                   {-# LINE 3270 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
          in  ( _lhsOcopy,_lhsOfree,_lhsOfresh,_lhsOinscope,_lhsOinvSubst)))
-- GuardedExpr -------------------------------------------------
-- cata
sem_GuardedExpr :: GuardedExpr ->
                   T_GuardedExpr
sem_GuardedExpr (GExpr _guard _expr) =
    (sem_GuardedExpr_GExpr (sem_Expr _guard) (sem_Expr _expr))
-- semantic domain
type T_GuardedExpr = Names ->
                     Names ->
                     Names ->
                     Subst ->
                     ( GuardedExpr,Names,Names,Subst)
sem_GuardedExpr_GExpr :: T_Expr ->
                         T_Expr ->
                         T_GuardedExpr
sem_GuardedExpr_GExpr guard_ expr_ =
    (\ _lhsIfixed
       _lhsIfresh
       _lhsIinscope
       _lhsIsubst ->
         (let _lhsOfree :: Names
              _lhsOinvSubst :: Subst
              _lhsOcopy :: GuardedExpr
              _lhsOfresh :: Names
              _guardOfixed :: Names
              _guardOfresh :: Names
              _guardOinscope :: Names
              _guardOsubst :: Subst
              _exprOfixed :: Names
              _exprOfresh :: Names
              _exprOinscope :: Names
              _exprOsubst :: Subst
              _guardIcopy :: Expr
              _guardIfree :: Names
              _guardIfresh :: Names
              _guardIinvSubst :: Subst
              _exprIcopy :: Expr
              _exprIfree :: Names
              _exprIfresh :: Names
              _exprIinvSubst :: Subst
              _lhsOfree =
                  ({-# LINE 103 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   (union _guardIfree _exprIfree)
                   {-# LINE 3316 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOinvSubst =
                  ({-# LINE 104 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   (M.union _guardIinvSubst _exprIinvSubst)
                   {-# LINE 3321 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _copy =
                  ({-# LINE 105 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   GExpr _guardIcopy _exprIcopy
                   {-# LINE 3326 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 105 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _copy
                   {-# LINE 3331 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 102 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _exprIfresh
                   {-# LINE 3336 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _guardOfixed =
                  ({-# LINE 101 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfixed
                   {-# LINE 3341 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _guardOfresh =
                  ({-# LINE 102 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfresh
                   {-# LINE 3346 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _guardOinscope =
                  ({-# LINE 99 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIinscope
                   {-# LINE 3351 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _guardOsubst =
                  ({-# LINE 100 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIsubst
                   {-# LINE 3356 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _exprOfixed =
                  ({-# LINE 101 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfixed
                   {-# LINE 3361 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _exprOfresh =
                  ({-# LINE 102 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _guardIfresh
                   {-# LINE 3366 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _exprOinscope =
                  ({-# LINE 99 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIinscope
                   {-# LINE 3371 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _exprOsubst =
                  ({-# LINE 100 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIsubst
                   {-# LINE 3376 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              ( _guardIcopy,_guardIfree,_guardIfresh,_guardIinvSubst) =
                  guard_ _guardOfixed _guardOfresh _guardOinscope _guardOsubst
              ( _exprIcopy,_exprIfree,_exprIfresh,_exprIinvSubst) =
                  expr_ _exprOfixed _exprOfresh _exprOinscope _exprOsubst
          in  ( _lhsOcopy,_lhsOfree,_lhsOfresh,_lhsOinvSubst)))
-- GuardedExprs ------------------------------------------------
-- cata
sem_GuardedExprs :: GuardedExprs ->
                    T_GuardedExprs
sem_GuardedExprs list =
    (Prelude.foldr sem_GuardedExprs_Cons sem_GuardedExprs_Nil (Prelude.map sem_GuardedExpr list))
-- semantic domain
type T_GuardedExprs = Names ->
                      Names ->
                      Names ->
                      Subst ->
                      ( GuardedExprs,Names,Names,Subst)
sem_GuardedExprs_Cons :: T_GuardedExpr ->
                         T_GuardedExprs ->
                         T_GuardedExprs
sem_GuardedExprs_Cons hd_ tl_ =
    (\ _lhsIfixed
       _lhsIfresh
       _lhsIinscope
       _lhsIsubst ->
         (let _lhsOfree :: Names
              _lhsOinvSubst :: Subst
              _lhsOcopy :: GuardedExprs
              _lhsOfresh :: Names
              _hdOfixed :: Names
              _hdOfresh :: Names
              _hdOinscope :: Names
              _hdOsubst :: Subst
              _tlOfixed :: Names
              _tlOfresh :: Names
              _tlOinscope :: Names
              _tlOsubst :: Subst
              _hdIcopy :: GuardedExpr
              _hdIfree :: Names
              _hdIfresh :: Names
              _hdIinvSubst :: Subst
              _tlIcopy :: GuardedExprs
              _tlIfree :: Names
              _tlIfresh :: Names
              _tlIinvSubst :: Subst
              _lhsOfree =
                  ({-# LINE 103 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   (union _hdIfree _tlIfree)
                   {-# LINE 3426 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOinvSubst =
                  ({-# LINE 104 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   (M.union _hdIinvSubst _tlIinvSubst)
                   {-# LINE 3431 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _copy =
                  ({-# LINE 105 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   (:) _hdIcopy _tlIcopy
                   {-# LINE 3436 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 105 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _copy
                   {-# LINE 3441 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 102 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _tlIfresh
                   {-# LINE 3446 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _hdOfixed =
                  ({-# LINE 101 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfixed
                   {-# LINE 3451 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _hdOfresh =
                  ({-# LINE 102 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfresh
                   {-# LINE 3456 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _hdOinscope =
                  ({-# LINE 99 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIinscope
                   {-# LINE 3461 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _hdOsubst =
                  ({-# LINE 100 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIsubst
                   {-# LINE 3466 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _tlOfixed =
                  ({-# LINE 101 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfixed
                   {-# LINE 3471 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _tlOfresh =
                  ({-# LINE 102 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _hdIfresh
                   {-# LINE 3476 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _tlOinscope =
                  ({-# LINE 99 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIinscope
                   {-# LINE 3481 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _tlOsubst =
                  ({-# LINE 100 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIsubst
                   {-# LINE 3486 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              ( _hdIcopy,_hdIfree,_hdIfresh,_hdIinvSubst) =
                  hd_ _hdOfixed _hdOfresh _hdOinscope _hdOsubst
              ( _tlIcopy,_tlIfree,_tlIfresh,_tlIinvSubst) =
                  tl_ _tlOfixed _tlOfresh _tlOinscope _tlOsubst
          in  ( _lhsOcopy,_lhsOfree,_lhsOfresh,_lhsOinvSubst)))
sem_GuardedExprs_Nil :: T_GuardedExprs
sem_GuardedExprs_Nil =
    (\ _lhsIfixed
       _lhsIfresh
       _lhsIinscope
       _lhsIsubst ->
         (let _lhsOfree :: Names
              _lhsOinvSubst :: Subst
              _lhsOcopy :: GuardedExprs
              _lhsOfresh :: Names
              _lhsOfree =
                  ({-# LINE 103 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   []
                   {-# LINE 3506 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOinvSubst =
                  ({-# LINE 104 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   M.empty
                   {-# LINE 3511 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _copy =
                  ({-# LINE 105 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   []
                   {-# LINE 3516 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 105 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _copy
                   {-# LINE 3521 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 102 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfresh
                   {-# LINE 3526 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
          in  ( _lhsOcopy,_lhsOfree,_lhsOfresh,_lhsOinvSubst)))
-- Literal -----------------------------------------------------
-- cata
sem_Literal :: Literal ->
               T_Literal
sem_Literal (LChar _val) =
    (sem_Literal_LChar _val)
sem_Literal (LFloat _val) =
    (sem_Literal_LFloat _val)
sem_Literal (LInt _val) =
    (sem_Literal_LInt _val)
sem_Literal (LString _val) =
    (sem_Literal_LString _val)
-- semantic domain
type T_Literal = ( Literal)
sem_Literal_LChar :: Char ->
                     T_Literal
sem_Literal_LChar val_ =
    (let _lhsOcopy :: Literal
         _copy =
             ({-# LINE 108 "src/Language/Haskell/Transformations/Alpha.ag" #-}
              LChar val_
              {-# LINE 3550 "src/Language/Haskell/Transformations/Alpha.hs" #-}
              )
         _lhsOcopy =
             ({-# LINE 108 "src/Language/Haskell/Transformations/Alpha.ag" #-}
              _copy
              {-# LINE 3555 "src/Language/Haskell/Transformations/Alpha.hs" #-}
              )
     in  ( _lhsOcopy))
sem_Literal_LFloat :: Float ->
                      T_Literal
sem_Literal_LFloat val_ =
    (let _lhsOcopy :: Literal
         _copy =
             ({-# LINE 108 "src/Language/Haskell/Transformations/Alpha.ag" #-}
              LFloat val_
              {-# LINE 3565 "src/Language/Haskell/Transformations/Alpha.hs" #-}
              )
         _lhsOcopy =
             ({-# LINE 108 "src/Language/Haskell/Transformations/Alpha.ag" #-}
              _copy
              {-# LINE 3570 "src/Language/Haskell/Transformations/Alpha.hs" #-}
              )
     in  ( _lhsOcopy))
sem_Literal_LInt :: Int ->
                    T_Literal
sem_Literal_LInt val_ =
    (let _lhsOcopy :: Literal
         _copy =
             ({-# LINE 108 "src/Language/Haskell/Transformations/Alpha.ag" #-}
              LInt val_
              {-# LINE 3580 "src/Language/Haskell/Transformations/Alpha.hs" #-}
              )
         _lhsOcopy =
             ({-# LINE 108 "src/Language/Haskell/Transformations/Alpha.ag" #-}
              _copy
              {-# LINE 3585 "src/Language/Haskell/Transformations/Alpha.hs" #-}
              )
     in  ( _lhsOcopy))
sem_Literal_LString :: String ->
                       T_Literal
sem_Literal_LString val_ =
    (let _lhsOcopy :: Literal
         _copy =
             ({-# LINE 108 "src/Language/Haskell/Transformations/Alpha.ag" #-}
              LString val_
              {-# LINE 3595 "src/Language/Haskell/Transformations/Alpha.hs" #-}
              )
         _lhsOcopy =
             ({-# LINE 108 "src/Language/Haskell/Transformations/Alpha.ag" #-}
              _copy
              {-# LINE 3600 "src/Language/Haskell/Transformations/Alpha.hs" #-}
              )
     in  ( _lhsOcopy))
-- MaybeExpr ---------------------------------------------------
-- cata
sem_MaybeExpr :: MaybeExpr ->
                 T_MaybeExpr
sem_MaybeExpr (NoExpr) =
    (sem_MaybeExpr_NoExpr)
sem_MaybeExpr (JustExpr _expr) =
    (sem_MaybeExpr_JustExpr (sem_Expr _expr))
-- semantic domain
type T_MaybeExpr = Names ->
                   Names ->
                   Names ->
                   Subst ->
                   ( MaybeExpr,Names,Names,Subst)
sem_MaybeExpr_NoExpr :: T_MaybeExpr
sem_MaybeExpr_NoExpr =
    (\ _lhsIfixed
       _lhsIfresh
       _lhsIinscope
       _lhsIsubst ->
         (let _lhsOfree :: Names
              _lhsOinvSubst :: Subst
              _lhsOcopy :: MaybeExpr
              _lhsOfresh :: Names
              _lhsOfree =
                  ({-# LINE 103 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   []
                   {-# LINE 3630 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOinvSubst =
                  ({-# LINE 104 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   M.empty
                   {-# LINE 3635 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _copy =
                  ({-# LINE 105 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   NoExpr
                   {-# LINE 3640 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 105 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _copy
                   {-# LINE 3645 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 102 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfresh
                   {-# LINE 3650 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
          in  ( _lhsOcopy,_lhsOfree,_lhsOfresh,_lhsOinvSubst)))
sem_MaybeExpr_JustExpr :: T_Expr ->
                          T_MaybeExpr
sem_MaybeExpr_JustExpr expr_ =
    (\ _lhsIfixed
       _lhsIfresh
       _lhsIinscope
       _lhsIsubst ->
         (let _lhsOfree :: Names
              _lhsOinvSubst :: Subst
              _lhsOcopy :: MaybeExpr
              _lhsOfresh :: Names
              _exprOfixed :: Names
              _exprOfresh :: Names
              _exprOinscope :: Names
              _exprOsubst :: Subst
              _exprIcopy :: Expr
              _exprIfree :: Names
              _exprIfresh :: Names
              _exprIinvSubst :: Subst
              _lhsOfree =
                  ({-# LINE 103 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _exprIfree
                   {-# LINE 3675 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOinvSubst =
                  ({-# LINE 104 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _exprIinvSubst
                   {-# LINE 3680 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _copy =
                  ({-# LINE 105 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   JustExpr _exprIcopy
                   {-# LINE 3685 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 105 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _copy
                   {-# LINE 3690 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 102 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _exprIfresh
                   {-# LINE 3695 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _exprOfixed =
                  ({-# LINE 101 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfixed
                   {-# LINE 3700 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _exprOfresh =
                  ({-# LINE 102 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfresh
                   {-# LINE 3705 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _exprOinscope =
                  ({-# LINE 99 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIinscope
                   {-# LINE 3710 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _exprOsubst =
                  ({-# LINE 100 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIsubst
                   {-# LINE 3715 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              ( _exprIcopy,_exprIfree,_exprIfresh,_exprIinvSubst) =
                  expr_ _exprOfixed _exprOfresh _exprOinscope _exprOsubst
          in  ( _lhsOcopy,_lhsOfree,_lhsOfresh,_lhsOinvSubst)))
-- MaybeName ---------------------------------------------------
-- cata
sem_MaybeName :: MaybeName ->
                 T_MaybeName
sem_MaybeName (NoName) =
    (sem_MaybeName_NoName)
sem_MaybeName (JustName _name) =
    (sem_MaybeName_JustName (sem_Name _name))
-- semantic domain
type T_MaybeName = ( MaybeName)
sem_MaybeName_NoName :: T_MaybeName
sem_MaybeName_NoName =
    (let _lhsOcopy :: MaybeName
         _copy =
             ({-# LINE 108 "src/Language/Haskell/Transformations/Alpha.ag" #-}
              NoName
              {-# LINE 3736 "src/Language/Haskell/Transformations/Alpha.hs" #-}
              )
         _lhsOcopy =
             ({-# LINE 108 "src/Language/Haskell/Transformations/Alpha.ag" #-}
              _copy
              {-# LINE 3741 "src/Language/Haskell/Transformations/Alpha.hs" #-}
              )
     in  ( _lhsOcopy))
sem_MaybeName_JustName :: T_Name ->
                          T_MaybeName
sem_MaybeName_JustName name_ =
    (let _lhsOcopy :: MaybeName
         _nameIcopy :: Name
         _copy =
             ({-# LINE 108 "src/Language/Haskell/Transformations/Alpha.ag" #-}
              JustName _nameIcopy
              {-# LINE 3752 "src/Language/Haskell/Transformations/Alpha.hs" #-}
              )
         _lhsOcopy =
             ({-# LINE 108 "src/Language/Haskell/Transformations/Alpha.ag" #-}
              _copy
              {-# LINE 3757 "src/Language/Haskell/Transformations/Alpha.hs" #-}
              )
         ( _nameIcopy) =
             name_
     in  ( _lhsOcopy))
-- Module ------------------------------------------------------
-- cata
sem_Module :: Module ->
              T_Module
sem_Module (Module _name _body) =
    (sem_Module_Module (sem_MaybeName _name) (sem_Body _body))
-- semantic domain
type T_Module = Names ->
                ( Module,Names,Subst)
sem_Module_Module :: T_MaybeName ->
                     T_Body ->
                     T_Module
sem_Module_Module name_ body_ =
    (\ _lhsIfixed ->
         (let _bodyOfresh :: Names
              _bodyOsubst :: Subst
              _bodyOinscope :: Names
              _lhsOfree :: Names
              _lhsOinvSubst :: Subst
              _lhsOcopy :: Module
              _bodyOfixed :: Names
              _nameIcopy :: MaybeName
              _bodyIcopy :: Body
              _bodyIfree :: Names
              _bodyIfresh :: Names
              _bodyIinscope :: Names
              _bodyIinvSubst :: Subst
              _bodyOfresh =
                  ({-# LINE 111 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   fresh \\ _bodyIfree
                   {-# LINE 3792 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _bodyOsubst =
                  ({-# LINE 112 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   M.empty
                   {-# LINE 3797 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _bodyOinscope =
                  ({-# LINE 113 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   []
                   {-# LINE 3802 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOfree =
                  ({-# LINE 77 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _bodyIfree
                   {-# LINE 3807 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOinvSubst =
                  ({-# LINE 78 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _bodyIinvSubst
                   {-# LINE 3812 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _copy =
                  ({-# LINE 79 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   Module _nameIcopy _bodyIcopy
                   {-# LINE 3817 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 79 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _copy
                   {-# LINE 3822 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _bodyOfixed =
                  ({-# LINE 83 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfixed
                   {-# LINE 3827 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              ( _nameIcopy) =
                  name_
              ( _bodyIcopy,_bodyIfree,_bodyIfresh,_bodyIinscope,_bodyIinvSubst) =
                  body_ _bodyOfixed _bodyOfresh _bodyOinscope _bodyOsubst
          in  ( _lhsOcopy,_lhsOfree,_lhsOinvSubst)))
-- Name --------------------------------------------------------
-- cata
sem_Name :: Name ->
            T_Name
sem_Name (Ident _name) =
    (sem_Name_Ident _name)
sem_Name (Operator _name) =
    (sem_Name_Operator _name)
sem_Name (Special _name) =
    (sem_Name_Special _name)
-- semantic domain
type T_Name = ( Name)
sem_Name_Ident :: String ->
                  T_Name
sem_Name_Ident name_ =
    (let _lhsOcopy :: Name
         _copy =
             ({-# LINE 108 "src/Language/Haskell/Transformations/Alpha.ag" #-}
              Ident name_
              {-# LINE 3853 "src/Language/Haskell/Transformations/Alpha.hs" #-}
              )
         _lhsOcopy =
             ({-# LINE 108 "src/Language/Haskell/Transformations/Alpha.ag" #-}
              _copy
              {-# LINE 3858 "src/Language/Haskell/Transformations/Alpha.hs" #-}
              )
     in  ( _lhsOcopy))
sem_Name_Operator :: String ->
                     T_Name
sem_Name_Operator name_ =
    (let _lhsOcopy :: Name
         _copy =
             ({-# LINE 108 "src/Language/Haskell/Transformations/Alpha.ag" #-}
              Operator name_
              {-# LINE 3868 "src/Language/Haskell/Transformations/Alpha.hs" #-}
              )
         _lhsOcopy =
             ({-# LINE 108 "src/Language/Haskell/Transformations/Alpha.ag" #-}
              _copy
              {-# LINE 3873 "src/Language/Haskell/Transformations/Alpha.hs" #-}
              )
     in  ( _lhsOcopy))
sem_Name_Special :: String ->
                    T_Name
sem_Name_Special name_ =
    (let _lhsOcopy :: Name
         _copy =
             ({-# LINE 108 "src/Language/Haskell/Transformations/Alpha.ag" #-}
              Special name_
              {-# LINE 3883 "src/Language/Haskell/Transformations/Alpha.hs" #-}
              )
         _lhsOcopy =
             ({-# LINE 108 "src/Language/Haskell/Transformations/Alpha.ag" #-}
              _copy
              {-# LINE 3888 "src/Language/Haskell/Transformations/Alpha.hs" #-}
              )
     in  ( _lhsOcopy))
-- Names -------------------------------------------------------
-- cata
sem_Names :: Names ->
             T_Names
sem_Names list =
    (Prelude.foldr sem_Names_Cons sem_Names_Nil (Prelude.map sem_Name list))
-- semantic domain
type T_Names = ( )
sem_Names_Cons :: T_Name ->
                  T_Names ->
                  T_Names
sem_Names_Cons hd_ tl_ =
    (let _hdIcopy :: Name
         ( _hdIcopy) =
             hd_
     in  ( ))
sem_Names_Nil :: T_Names
sem_Names_Nil =
    (let
     in  ( ))
-- Pat ---------------------------------------------------------
-- cata
sem_Pat :: Pat ->
           T_Pat
sem_Pat (PHole _id) =
    (sem_Pat_PHole _id)
sem_Pat (PMultipleHole _id) =
    (sem_Pat_PMultipleHole _id)
sem_Pat (PCon _name _pats) =
    (sem_Pat_PCon (sem_Name _name) (sem_Pats _pats))
sem_Pat (PInfixCon _left _name _right) =
    (sem_Pat_PInfixCon (sem_Pat _left) (sem_Name _name) (sem_Pat _right))
sem_Pat (PList _pats) =
    (sem_Pat_PList (sem_Pats _pats))
sem_Pat (PLit _lit) =
    (sem_Pat_PLit (sem_Literal _lit))
sem_Pat (PParen _pat) =
    (sem_Pat_PParen (sem_Pat _pat))
sem_Pat (PTuple _pats) =
    (sem_Pat_PTuple (sem_Pats _pats))
sem_Pat (PVar _name) =
    (sem_Pat_PVar (sem_Name _name))
sem_Pat (PAs _name _pat) =
    (sem_Pat_PAs (sem_Name _name) (sem_Pat _pat))
sem_Pat (PWildcard) =
    (sem_Pat_PWildcard)
-- semantic domain
type T_Pat = Names ->
             Subst ->
             ( Pat,Names,Subst)
sem_Pat_PHole :: HoleID ->
                 T_Pat
sem_Pat_PHole id_ =
    (\ _lhsIfixed
       _lhsIsubst ->
         (let _lhsOinscope :: Names
              _lhsOinvSubst :: Subst
              _lhsOcopy :: Pat
              _lhsOinscope =
                  ({-# LINE 94 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   []
                   {-# LINE 3952 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOinvSubst =
                  ({-# LINE 95 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   M.empty
                   {-# LINE 3957 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _copy =
                  ({-# LINE 96 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   PHole id_
                   {-# LINE 3962 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 96 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _copy
                   {-# LINE 3967 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
          in  ( _lhsOcopy,_lhsOinscope,_lhsOinvSubst)))
sem_Pat_PMultipleHole :: HoleID ->
                         T_Pat
sem_Pat_PMultipleHole id_ =
    (\ _lhsIfixed
       _lhsIsubst ->
         (let _lhsOinscope :: Names
              _lhsOinvSubst :: Subst
              _lhsOcopy :: Pat
              _lhsOinscope =
                  ({-# LINE 94 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   []
                   {-# LINE 3981 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOinvSubst =
                  ({-# LINE 95 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   M.empty
                   {-# LINE 3986 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _copy =
                  ({-# LINE 96 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   PMultipleHole id_
                   {-# LINE 3991 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 96 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _copy
                   {-# LINE 3996 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
          in  ( _lhsOcopy,_lhsOinscope,_lhsOinvSubst)))
sem_Pat_PCon :: T_Name ->
                T_Pats ->
                T_Pat
sem_Pat_PCon name_ pats_ =
    (\ _lhsIfixed
       _lhsIsubst ->
         (let _lhsOinscope :: Names
              _lhsOinvSubst :: Subst
              _lhsOcopy :: Pat
              _patsOfixed :: Names
              _patsOsubst :: Subst
              _nameIcopy :: Name
              _patsIcopy :: Pats
              _patsIinscope :: Names
              _patsIinvSubst :: Subst
              _lhsOinscope =
                  ({-# LINE 94 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _patsIinscope
                   {-# LINE 4017 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOinvSubst =
                  ({-# LINE 95 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _patsIinvSubst
                   {-# LINE 4022 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _copy =
                  ({-# LINE 96 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   PCon _nameIcopy _patsIcopy
                   {-# LINE 4027 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 96 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _copy
                   {-# LINE 4032 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _patsOfixed =
                  ({-# LINE 93 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfixed
                   {-# LINE 4037 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _patsOsubst =
                  ({-# LINE 92 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIsubst
                   {-# LINE 4042 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              ( _nameIcopy) =
                  name_
              ( _patsIcopy,_patsIinscope,_patsIinvSubst) =
                  pats_ _patsOfixed _patsOsubst
          in  ( _lhsOcopy,_lhsOinscope,_lhsOinvSubst)))
sem_Pat_PInfixCon :: T_Pat ->
                     T_Name ->
                     T_Pat ->
                     T_Pat
sem_Pat_PInfixCon left_ name_ right_ =
    (\ _lhsIfixed
       _lhsIsubst ->
         (let _lhsOinscope :: Names
              _lhsOinvSubst :: Subst
              _lhsOcopy :: Pat
              _leftOfixed :: Names
              _leftOsubst :: Subst
              _rightOfixed :: Names
              _rightOsubst :: Subst
              _leftIcopy :: Pat
              _leftIinscope :: Names
              _leftIinvSubst :: Subst
              _nameIcopy :: Name
              _rightIcopy :: Pat
              _rightIinscope :: Names
              _rightIinvSubst :: Subst
              _lhsOinscope =
                  ({-# LINE 94 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   (union _leftIinscope _rightIinscope)
                   {-# LINE 4073 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOinvSubst =
                  ({-# LINE 95 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   (M.union _leftIinvSubst _rightIinvSubst)
                   {-# LINE 4078 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _copy =
                  ({-# LINE 96 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   PInfixCon _leftIcopy _nameIcopy _rightIcopy
                   {-# LINE 4083 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 96 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _copy
                   {-# LINE 4088 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _leftOfixed =
                  ({-# LINE 93 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfixed
                   {-# LINE 4093 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _leftOsubst =
                  ({-# LINE 92 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIsubst
                   {-# LINE 4098 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _rightOfixed =
                  ({-# LINE 93 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfixed
                   {-# LINE 4103 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _rightOsubst =
                  ({-# LINE 92 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIsubst
                   {-# LINE 4108 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              ( _leftIcopy,_leftIinscope,_leftIinvSubst) =
                  left_ _leftOfixed _leftOsubst
              ( _nameIcopy) =
                  name_
              ( _rightIcopy,_rightIinscope,_rightIinvSubst) =
                  right_ _rightOfixed _rightOsubst
          in  ( _lhsOcopy,_lhsOinscope,_lhsOinvSubst)))
sem_Pat_PList :: T_Pats ->
                 T_Pat
sem_Pat_PList pats_ =
    (\ _lhsIfixed
       _lhsIsubst ->
         (let _lhsOinscope :: Names
              _lhsOinvSubst :: Subst
              _lhsOcopy :: Pat
              _patsOfixed :: Names
              _patsOsubst :: Subst
              _patsIcopy :: Pats
              _patsIinscope :: Names
              _patsIinvSubst :: Subst
              _lhsOinscope =
                  ({-# LINE 94 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _patsIinscope
                   {-# LINE 4133 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOinvSubst =
                  ({-# LINE 95 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _patsIinvSubst
                   {-# LINE 4138 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _copy =
                  ({-# LINE 96 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   PList _patsIcopy
                   {-# LINE 4143 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 96 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _copy
                   {-# LINE 4148 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _patsOfixed =
                  ({-# LINE 93 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfixed
                   {-# LINE 4153 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _patsOsubst =
                  ({-# LINE 92 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIsubst
                   {-# LINE 4158 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              ( _patsIcopy,_patsIinscope,_patsIinvSubst) =
                  pats_ _patsOfixed _patsOsubst
          in  ( _lhsOcopy,_lhsOinscope,_lhsOinvSubst)))
sem_Pat_PLit :: T_Literal ->
                T_Pat
sem_Pat_PLit lit_ =
    (\ _lhsIfixed
       _lhsIsubst ->
         (let _lhsOinscope :: Names
              _lhsOinvSubst :: Subst
              _lhsOcopy :: Pat
              _litIcopy :: Literal
              _lhsOinscope =
                  ({-# LINE 94 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   []
                   {-# LINE 4175 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOinvSubst =
                  ({-# LINE 95 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   M.empty
                   {-# LINE 4180 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _copy =
                  ({-# LINE 96 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   PLit _litIcopy
                   {-# LINE 4185 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 96 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _copy
                   {-# LINE 4190 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              ( _litIcopy) =
                  lit_
          in  ( _lhsOcopy,_lhsOinscope,_lhsOinvSubst)))
sem_Pat_PParen :: T_Pat ->
                  T_Pat
sem_Pat_PParen pat_ =
    (\ _lhsIfixed
       _lhsIsubst ->
         (let _lhsOinscope :: Names
              _lhsOinvSubst :: Subst
              _lhsOcopy :: Pat
              _patOfixed :: Names
              _patOsubst :: Subst
              _patIcopy :: Pat
              _patIinscope :: Names
              _patIinvSubst :: Subst
              _lhsOinscope =
                  ({-# LINE 94 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _patIinscope
                   {-# LINE 4211 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOinvSubst =
                  ({-# LINE 95 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _patIinvSubst
                   {-# LINE 4216 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _copy =
                  ({-# LINE 96 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   PParen _patIcopy
                   {-# LINE 4221 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 96 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _copy
                   {-# LINE 4226 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _patOfixed =
                  ({-# LINE 93 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfixed
                   {-# LINE 4231 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _patOsubst =
                  ({-# LINE 92 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIsubst
                   {-# LINE 4236 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              ( _patIcopy,_patIinscope,_patIinvSubst) =
                  pat_ _patOfixed _patOsubst
          in  ( _lhsOcopy,_lhsOinscope,_lhsOinvSubst)))
sem_Pat_PTuple :: T_Pats ->
                  T_Pat
sem_Pat_PTuple pats_ =
    (\ _lhsIfixed
       _lhsIsubst ->
         (let _lhsOinscope :: Names
              _lhsOinvSubst :: Subst
              _lhsOcopy :: Pat
              _patsOfixed :: Names
              _patsOsubst :: Subst
              _patsIcopy :: Pats
              _patsIinscope :: Names
              _patsIinvSubst :: Subst
              _lhsOinscope =
                  ({-# LINE 94 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _patsIinscope
                   {-# LINE 4257 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOinvSubst =
                  ({-# LINE 95 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _patsIinvSubst
                   {-# LINE 4262 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _copy =
                  ({-# LINE 96 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   PTuple _patsIcopy
                   {-# LINE 4267 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 96 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _copy
                   {-# LINE 4272 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _patsOfixed =
                  ({-# LINE 93 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfixed
                   {-# LINE 4277 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _patsOsubst =
                  ({-# LINE 92 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIsubst
                   {-# LINE 4282 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              ( _patsIcopy,_patsIinscope,_patsIinvSubst) =
                  pats_ _patsOfixed _patsOsubst
          in  ( _lhsOcopy,_lhsOinscope,_lhsOinvSubst)))
sem_Pat_PVar :: T_Name ->
                T_Pat
sem_Pat_PVar name_ =
    (\ _lhsIfixed
       _lhsIsubst ->
         (let _lhsOinscope :: Names
              _lhsOcopy :: Pat
              _lhsOinvSubst :: Subst
              _nameIcopy :: Name
              _lhsOinscope =
                  ({-# LINE 179 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   [_nameIcopy]
                   {-# LINE 4299 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 180 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   PVar $ substitute _nameIcopy _lhsIsubst
                   {-# LINE 4304 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOinvSubst =
                  ({-# LINE 181 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   inverseSubst _lhsIsubst
                   {-# LINE 4309 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _copy =
                  ({-# LINE 96 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   PVar _nameIcopy
                   {-# LINE 4314 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              ( _nameIcopy) =
                  name_
          in  ( _lhsOcopy,_lhsOinscope,_lhsOinvSubst)))
sem_Pat_PAs :: T_Name ->
               T_Pat ->
               T_Pat
sem_Pat_PAs name_ pat_ =
    (\ _lhsIfixed
       _lhsIsubst ->
         (let _lhsOinscope :: Names
              _lhsOcopy :: Pat
              _lhsOinvSubst :: Subst
              _patOfixed :: Names
              _patOsubst :: Subst
              _nameIcopy :: Name
              _patIcopy :: Pat
              _patIinscope :: Names
              _patIinvSubst :: Subst
              _lhsOinscope =
                  ({-# LINE 182 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _nameIcopy : _patIinscope
                   {-# LINE 4337 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 183 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   PAs (substitute _nameIcopy _lhsIsubst) _patIcopy
                   {-# LINE 4342 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOinvSubst =
                  ({-# LINE 184 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   inverseSubst _lhsIsubst
                   {-# LINE 4347 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _copy =
                  ({-# LINE 96 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   PAs _nameIcopy _patIcopy
                   {-# LINE 4352 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _patOfixed =
                  ({-# LINE 93 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfixed
                   {-# LINE 4357 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _patOsubst =
                  ({-# LINE 92 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIsubst
                   {-# LINE 4362 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              ( _nameIcopy) =
                  name_
              ( _patIcopy,_patIinscope,_patIinvSubst) =
                  pat_ _patOfixed _patOsubst
          in  ( _lhsOcopy,_lhsOinscope,_lhsOinvSubst)))
sem_Pat_PWildcard :: T_Pat
sem_Pat_PWildcard =
    (\ _lhsIfixed
       _lhsIsubst ->
         (let _lhsOinscope :: Names
              _lhsOinvSubst :: Subst
              _lhsOcopy :: Pat
              _lhsOinscope =
                  ({-# LINE 185 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   [Special ""]
                   {-# LINE 4379 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOinvSubst =
                  ({-# LINE 95 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   M.empty
                   {-# LINE 4384 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _copy =
                  ({-# LINE 96 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   PWildcard
                   {-# LINE 4389 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 96 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _copy
                   {-# LINE 4394 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
          in  ( _lhsOcopy,_lhsOinscope,_lhsOinvSubst)))
-- Pats --------------------------------------------------------
-- cata
sem_Pats :: Pats ->
            T_Pats
sem_Pats list =
    (Prelude.foldr sem_Pats_Cons sem_Pats_Nil (Prelude.map sem_Pat list))
-- semantic domain
type T_Pats = Names ->
              Subst ->
              ( Pats,Names,Subst)
sem_Pats_Cons :: T_Pat ->
                 T_Pats ->
                 T_Pats
sem_Pats_Cons hd_ tl_ =
    (\ _lhsIfixed
       _lhsIsubst ->
         (let _lhsOinscope :: Names
              _lhsOinvSubst :: Subst
              _lhsOcopy :: Pats
              _hdOfixed :: Names
              _hdOsubst :: Subst
              _tlOfixed :: Names
              _tlOsubst :: Subst
              _hdIcopy :: Pat
              _hdIinscope :: Names
              _hdIinvSubst :: Subst
              _tlIcopy :: Pats
              _tlIinscope :: Names
              _tlIinvSubst :: Subst
              _lhsOinscope =
                  ({-# LINE 94 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   (union _hdIinscope _tlIinscope)
                   {-# LINE 4429 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOinvSubst =
                  ({-# LINE 95 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   (M.union _hdIinvSubst _tlIinvSubst)
                   {-# LINE 4434 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _copy =
                  ({-# LINE 96 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   (:) _hdIcopy _tlIcopy
                   {-# LINE 4439 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 96 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _copy
                   {-# LINE 4444 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _hdOfixed =
                  ({-# LINE 93 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfixed
                   {-# LINE 4449 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _hdOsubst =
                  ({-# LINE 92 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIsubst
                   {-# LINE 4454 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _tlOfixed =
                  ({-# LINE 93 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfixed
                   {-# LINE 4459 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _tlOsubst =
                  ({-# LINE 92 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIsubst
                   {-# LINE 4464 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              ( _hdIcopy,_hdIinscope,_hdIinvSubst) =
                  hd_ _hdOfixed _hdOsubst
              ( _tlIcopy,_tlIinscope,_tlIinvSubst) =
                  tl_ _tlOfixed _tlOsubst
          in  ( _lhsOcopy,_lhsOinscope,_lhsOinvSubst)))
sem_Pats_Nil :: T_Pats
sem_Pats_Nil =
    (\ _lhsIfixed
       _lhsIsubst ->
         (let _lhsOinscope :: Names
              _lhsOinvSubst :: Subst
              _lhsOcopy :: Pats
              _lhsOinscope =
                  ({-# LINE 94 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   []
                   {-# LINE 4481 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOinvSubst =
                  ({-# LINE 95 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   M.empty
                   {-# LINE 4486 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _copy =
                  ({-# LINE 96 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   []
                   {-# LINE 4491 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 96 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _copy
                   {-# LINE 4496 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
          in  ( _lhsOcopy,_lhsOinscope,_lhsOinvSubst)))
-- RefactorChoice ----------------------------------------------
-- cata
sem_RefactorChoice :: RefactorChoice ->
                      T_RefactorChoice
sem_RefactorChoice (RefactorChoice _message _expr) =
    (sem_RefactorChoice_RefactorChoice _message (sem_Expr _expr))
-- semantic domain
type T_RefactorChoice = Names ->
                        Names ->
                        Names ->
                        Subst ->
                        ( RefactorChoice,Names,Names,Subst)
sem_RefactorChoice_RefactorChoice :: String ->
                                     T_Expr ->
                                     T_RefactorChoice
sem_RefactorChoice_RefactorChoice message_ expr_ =
    (\ _lhsIfixed
       _lhsIfresh
       _lhsIinscope
       _lhsIsubst ->
         (let _lhsOfree :: Names
              _lhsOinvSubst :: Subst
              _lhsOcopy :: RefactorChoice
              _lhsOfresh :: Names
              _exprOfixed :: Names
              _exprOfresh :: Names
              _exprOinscope :: Names
              _exprOsubst :: Subst
              _exprIcopy :: Expr
              _exprIfree :: Names
              _exprIfresh :: Names
              _exprIinvSubst :: Subst
              _lhsOfree =
                  ({-# LINE 103 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _exprIfree
                   {-# LINE 4534 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOinvSubst =
                  ({-# LINE 104 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _exprIinvSubst
                   {-# LINE 4539 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _copy =
                  ({-# LINE 105 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   RefactorChoice message_ _exprIcopy
                   {-# LINE 4544 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 105 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _copy
                   {-# LINE 4549 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 102 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _exprIfresh
                   {-# LINE 4554 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _exprOfixed =
                  ({-# LINE 101 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfixed
                   {-# LINE 4559 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _exprOfresh =
                  ({-# LINE 102 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfresh
                   {-# LINE 4564 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _exprOinscope =
                  ({-# LINE 99 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIinscope
                   {-# LINE 4569 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _exprOsubst =
                  ({-# LINE 100 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIsubst
                   {-# LINE 4574 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              ( _exprIcopy,_exprIfree,_exprIfresh,_exprIinvSubst) =
                  expr_ _exprOfixed _exprOfresh _exprOinscope _exprOsubst
          in  ( _lhsOcopy,_lhsOfree,_lhsOfresh,_lhsOinvSubst)))
-- RefactorChoices ---------------------------------------------
-- cata
sem_RefactorChoices :: RefactorChoices ->
                       T_RefactorChoices
sem_RefactorChoices list =
    (Prelude.foldr sem_RefactorChoices_Cons sem_RefactorChoices_Nil (Prelude.map sem_RefactorChoice list))
-- semantic domain
type T_RefactorChoices = Names ->
                         Names ->
                         Names ->
                         Subst ->
                         ( RefactorChoices,Names,Names,Subst)
sem_RefactorChoices_Cons :: T_RefactorChoice ->
                            T_RefactorChoices ->
                            T_RefactorChoices
sem_RefactorChoices_Cons hd_ tl_ =
    (\ _lhsIfixed
       _lhsIfresh
       _lhsIinscope
       _lhsIsubst ->
         (let _lhsOfree :: Names
              _lhsOinvSubst :: Subst
              _lhsOcopy :: RefactorChoices
              _lhsOfresh :: Names
              _hdOfixed :: Names
              _hdOfresh :: Names
              _hdOinscope :: Names
              _hdOsubst :: Subst
              _tlOfixed :: Names
              _tlOfresh :: Names
              _tlOinscope :: Names
              _tlOsubst :: Subst
              _hdIcopy :: RefactorChoice
              _hdIfree :: Names
              _hdIfresh :: Names
              _hdIinvSubst :: Subst
              _tlIcopy :: RefactorChoices
              _tlIfree :: Names
              _tlIfresh :: Names
              _tlIinvSubst :: Subst
              _lhsOfree =
                  ({-# LINE 103 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   (union _hdIfree _tlIfree)
                   {-# LINE 4622 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOinvSubst =
                  ({-# LINE 104 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   (M.union _hdIinvSubst _tlIinvSubst)
                   {-# LINE 4627 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _copy =
                  ({-# LINE 105 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   (:) _hdIcopy _tlIcopy
                   {-# LINE 4632 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 105 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _copy
                   {-# LINE 4637 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 102 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _tlIfresh
                   {-# LINE 4642 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _hdOfixed =
                  ({-# LINE 101 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfixed
                   {-# LINE 4647 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _hdOfresh =
                  ({-# LINE 102 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfresh
                   {-# LINE 4652 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _hdOinscope =
                  ({-# LINE 99 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIinscope
                   {-# LINE 4657 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _hdOsubst =
                  ({-# LINE 100 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIsubst
                   {-# LINE 4662 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _tlOfixed =
                  ({-# LINE 101 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfixed
                   {-# LINE 4667 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _tlOfresh =
                  ({-# LINE 102 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _hdIfresh
                   {-# LINE 4672 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _tlOinscope =
                  ({-# LINE 99 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIinscope
                   {-# LINE 4677 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _tlOsubst =
                  ({-# LINE 100 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIsubst
                   {-# LINE 4682 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              ( _hdIcopy,_hdIfree,_hdIfresh,_hdIinvSubst) =
                  hd_ _hdOfixed _hdOfresh _hdOinscope _hdOsubst
              ( _tlIcopy,_tlIfree,_tlIfresh,_tlIinvSubst) =
                  tl_ _tlOfixed _tlOfresh _tlOinscope _tlOsubst
          in  ( _lhsOcopy,_lhsOfree,_lhsOfresh,_lhsOinvSubst)))
sem_RefactorChoices_Nil :: T_RefactorChoices
sem_RefactorChoices_Nil =
    (\ _lhsIfixed
       _lhsIfresh
       _lhsIinscope
       _lhsIsubst ->
         (let _lhsOfree :: Names
              _lhsOinvSubst :: Subst
              _lhsOcopy :: RefactorChoices
              _lhsOfresh :: Names
              _lhsOfree =
                  ({-# LINE 103 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   []
                   {-# LINE 4702 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOinvSubst =
                  ({-# LINE 104 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   M.empty
                   {-# LINE 4707 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _copy =
                  ({-# LINE 105 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   []
                   {-# LINE 4712 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 105 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _copy
                   {-# LINE 4717 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 102 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfresh
                   {-# LINE 4722 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
          in  ( _lhsOcopy,_lhsOfree,_lhsOfresh,_lhsOinvSubst)))
-- Rhs ---------------------------------------------------------
-- cata
sem_Rhs :: Rhs ->
           T_Rhs
sem_Rhs (Rhs _expr _where) =
    (sem_Rhs_Rhs (sem_Expr _expr) (sem_Decls _where))
sem_Rhs (GRhs _gexprs _where) =
    (sem_Rhs_GRhs (sem_GuardedExprs _gexprs) (sem_Decls _where))
-- semantic domain
type T_Rhs = Names ->
             Names ->
             Names ->
             Subst ->
             ( Rhs,Names,Names,Subst)
sem_Rhs_Rhs :: T_Expr ->
               T_Decls ->
               T_Rhs
sem_Rhs_Rhs expr_ where_ =
    (\ _lhsIfixed
       _lhsIfresh
       _lhsIinscope
       _lhsIsubst ->
         (let _exprOinscope :: Names
              _exprOsubst :: Subst
              _exprOfresh :: Names
              _whereOfresh :: Names
              _whereOsubst :: Subst
              _whereOinscope :: Names
              _lhsOfree :: Names
              _lhsOfresh :: Names
              _lhsOinvSubst :: Subst
              _lhsOcopy :: Rhs
              _exprOfixed :: Names
              _whereOfixed :: Names
              _exprIcopy :: Expr
              _exprIfree :: Names
              _exprIfresh :: Names
              _exprIinvSubst :: Subst
              _whereIcopy :: Decls
              _whereIfree :: Names
              _whereIfresh :: Names
              _whereIinscope :: Names
              _whereIinvSubst :: Subst
              _exprOinscope =
                  ({-# LINE 188 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIinscope `union` _inscope
                   {-# LINE 4771 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _exprOsubst =
                  ({-# LINE 189 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _subst
                   {-# LINE 4776 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _exprOfresh =
                  ({-# LINE 190 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _whereIfresh
                   {-# LINE 4781 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _whereOfresh =
                  ({-# LINE 191 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   drop (length _inscope) _lhsIfresh
                   {-# LINE 4786 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _whereOsubst =
                  ({-# LINE 192 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _subst
                   {-# LINE 4791 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _whereOinscope =
                  ({-# LINE 193 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _inscope `union` _lhsIinscope
                   {-# LINE 4796 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOfree =
                  ({-# LINE 194 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _exprIfree \\ _whereIinscope
                   {-# LINE 4801 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 195 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _exprIfresh
                   {-# LINE 4806 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _subst =
                  ({-# LINE 196 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   updateSubst _inscope _lhsIfresh _lhsIsubst
                   {-# LINE 4811 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _inscope =
                  ({-# LINE 197 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _whereIinscope \\ _lhsIfixed
                   {-# LINE 4816 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOinvSubst =
                  ({-# LINE 104 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   (M.union _exprIinvSubst _whereIinvSubst)
                   {-# LINE 4821 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _copy =
                  ({-# LINE 105 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   Rhs _exprIcopy _whereIcopy
                   {-# LINE 4826 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 105 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _copy
                   {-# LINE 4831 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _exprOfixed =
                  ({-# LINE 101 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfixed
                   {-# LINE 4836 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _whereOfixed =
                  ({-# LINE 83 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfixed
                   {-# LINE 4841 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              ( _exprIcopy,_exprIfree,_exprIfresh,_exprIinvSubst) =
                  expr_ _exprOfixed _exprOfresh _exprOinscope _exprOsubst
              ( _whereIcopy,_whereIfree,_whereIfresh,_whereIinscope,_whereIinvSubst) =
                  where_ _whereOfixed _whereOfresh _whereOinscope _whereOsubst
          in  ( _lhsOcopy,_lhsOfree,_lhsOfresh,_lhsOinvSubst)))
sem_Rhs_GRhs :: T_GuardedExprs ->
                T_Decls ->
                T_Rhs
sem_Rhs_GRhs gexprs_ where_ =
    (\ _lhsIfixed
       _lhsIfresh
       _lhsIinscope
       _lhsIsubst ->
         (let _gexprsOinscope :: Names
              _gexprsOsubst :: Subst
              _gexprsOfresh :: Names
              _whereOfresh :: Names
              _whereOsubst :: Subst
              _whereOinscope :: Names
              _lhsOfree :: Names
              _lhsOfresh :: Names
              _lhsOinvSubst :: Subst
              _lhsOcopy :: Rhs
              _gexprsOfixed :: Names
              _whereOfixed :: Names
              _gexprsIcopy :: GuardedExprs
              _gexprsIfree :: Names
              _gexprsIfresh :: Names
              _gexprsIinvSubst :: Subst
              _whereIcopy :: Decls
              _whereIfree :: Names
              _whereIfresh :: Names
              _whereIinscope :: Names
              _whereIinvSubst :: Subst
              _gexprsOinscope =
                  ({-# LINE 199 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIinscope `union` _inscope
                   {-# LINE 4880 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _gexprsOsubst =
                  ({-# LINE 200 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _subst
                   {-# LINE 4885 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _gexprsOfresh =
                  ({-# LINE 201 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _whereIfresh
                   {-# LINE 4890 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _whereOfresh =
                  ({-# LINE 202 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   drop (length _inscope) _lhsIfresh
                   {-# LINE 4895 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _whereOsubst =
                  ({-# LINE 203 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _subst
                   {-# LINE 4900 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _whereOinscope =
                  ({-# LINE 204 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _inscope `union` _lhsIinscope
                   {-# LINE 4905 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOfree =
                  ({-# LINE 205 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _gexprsIfree \\ _whereIinscope
                   {-# LINE 4910 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOfresh =
                  ({-# LINE 206 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _gexprsIfresh
                   {-# LINE 4915 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _subst =
                  ({-# LINE 207 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   updateSubst _inscope _lhsIfresh _lhsIsubst
                   {-# LINE 4920 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _inscope =
                  ({-# LINE 208 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _whereIinscope \\ _lhsIfixed
                   {-# LINE 4925 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOinvSubst =
                  ({-# LINE 104 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   (M.union _gexprsIinvSubst _whereIinvSubst)
                   {-# LINE 4930 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _copy =
                  ({-# LINE 105 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   GRhs _gexprsIcopy _whereIcopy
                   {-# LINE 4935 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _lhsOcopy =
                  ({-# LINE 105 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _copy
                   {-# LINE 4940 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _gexprsOfixed =
                  ({-# LINE 101 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfixed
                   {-# LINE 4945 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              _whereOfixed =
                  ({-# LINE 83 "src/Language/Haskell/Transformations/Alpha.ag" #-}
                   _lhsIfixed
                   {-# LINE 4950 "src/Language/Haskell/Transformations/Alpha.hs" #-}
                   )
              ( _gexprsIcopy,_gexprsIfree,_gexprsIfresh,_gexprsIinvSubst) =
                  gexprs_ _gexprsOfixed _gexprsOfresh _gexprsOinscope _gexprsOsubst
              ( _whereIcopy,_whereIfree,_whereIfresh,_whereIinscope,_whereIinvSubst) =
                  where_ _whereOfixed _whereOfresh _whereOinscope _whereOsubst
          in  ( _lhsOcopy,_lhsOfree,_lhsOfresh,_lhsOinvSubst)))
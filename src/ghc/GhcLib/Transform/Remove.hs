module GhcLib.Transform.Remove where

import GHC.Core qualified as GHC
import GHC.Core.Opt.Arity qualified as GHC
import GHC.Core.Predicate qualified as GHC
import GHC.Core.Type qualified as GHC
import GHC.Core.Utils qualified as GHC
import GHC.Types.Id qualified as GHC
import GHC.Types.Id.Info qualified as GHC
import GHC.Types.Name qualified as GHCOcc
import GHC.Types.SrcLoc qualified as GHC
import GHC.Types.Unique qualified as GHC
import GHC.Types.Unique.Supply qualified as GHC
import GHC.Types.Var qualified as GHC

import Control.Monad.State
import Data.Data
import Data.Generics.Uniplate.Data
import Data.Map qualified as Map
import Data.Maybe (fromJust)

import GhcLib.Transform.Utility

removeRedundantEqCheck :: GHC.CoreProgram -> GHC.CoreProgram

-- | Remove redundant equality checks
removeRedundantEqCheck = transformBi remEqCheck
  where
    remEqCheck :: GHC.CoreExpr -> GHC.CoreExpr
    -- \| remove redundant boolean checks, e.g. removeRedEqCheck
    -- if x == y then true else false ==> x == y
    -- f x y | x == y =    True
    --       | otherwise = False      ==> x == y
    remEqCheck (GHC.Case e _v _t alt)
        | isEqCheck e || isNeqCheck e
        , all isBoolToBool alt =
            e
        | isEqCheck e
        , all isNegBoolToBool alt =
            replace "==" "/=" e
        | isNeqCheck e
        , all isNegBoolToBool alt =
            replace "/=" "==" e
    remEqCheck e = e
    replace :: String -> String -> GHC.CoreExpr -> GHC.CoreExpr
    -- \| Replace the variable name with another name, and
    --   update all occurences of the variable in the given expression
    replace old new e = subsVar vnew vold e
      where
        vold = fromJust $ getVarFromName old e
        vnew =
            GHC.setVarName vold (makeName new (GHC.getUnique vold) (GHCOcc.getSrcSpan vold))

isEqCheck :: (Data GHC.Var) => GHC.CoreExpr -> Bool
isEqCheck e = or [GHCOcc.getOccString v == "==" | GHC.Var v <- universe e]

isNeqCheck :: (Data GHC.Var) => GHC.CoreExpr -> Bool
isNeqCheck e = or [GHCOcc.getOccString v == "/=" | GHC.Var v <- universe e]

-- ! This style of checking will probably woundn't work, as the GHC.Var v is often wrapped inside Tick
isBoolToBool :: GHC.Alt GHC.Var -> Bool

-- | GHC.Case on a bool that also returns a bool
isBoolToBool (GHC.Alt (GHC.DataAlt d) [] (GHC.Var v)) = (dstr == "False" || dstr == "True") && dstr == vstr
  where
    dstr = GHCOcc.getOccString d
    vstr = GHCOcc.getOccString v
isBoolToBool _ = False

isNegBoolToBool :: GHC.Alt GHC.Var -> Bool

-- | GHC.Case on a bool that also returns a bool, but with reversed logic
isNegBoolToBool (GHC.Alt (GHC.DataAlt d) [] (GHC.Var v)) =
    ( dstr == "False"
        && vstr == "True"
    )
        || ( dstr == "True"
                && vstr == "False"
           )
  where
    dstr = GHCOcc.getOccString d
    vstr = GHCOcc.getOccString v
isNegBoolToBool _ = False

removeTyEvidence :: GHC.CoreProgram -> GHC.CoreProgram

-- | Remove types and type evidence from a Coreprogram
removeTyEvidence = transformBi removeTy
  where
    removeTy = \case
        (GHC.Lam v e) | GHC.isEvVar v || GHC.isTyVar v -> e
        (GHC.App f (GHC.Var v)) | GHC.isEvVar v || GHC.isTyVar v -> f
        (GHC.App f (GHC.Type _t)) -> f
        (GHC.Let b e) | isEvBind b -> e
        e -> e
    isEvBind (GHC.NonRec bi e) = isEvOrTyVar bi && isEvOrTyExp e
    isEvBind (GHC.Rec es) = all (isEvOrTyVar . fst) es && all (isEvOrTyExp . snd) es

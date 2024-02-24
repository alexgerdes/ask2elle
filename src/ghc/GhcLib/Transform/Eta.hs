module GhcLib.Transform.Eta where


import qualified GHC.Plugins as GHC
import qualified GHC.Types.Name.Occurrence as Occ
import qualified GHC.Utils.Encoding as GHC
import qualified GHC.Core.Predicate as GHC 
-- General imports 
import Data.Generics.Uniplate.Data
import Control.Monad ( void, replicateM_ )

import qualified Text.Megaparsec as Parser
import qualified Text.Megaparsec.Char as Parser
import Data.Void (Void)
import GHC.Base (assert)
import Data.Foldable (Foldable(foldl'))
import Data.Bifunctor (Bifunctor(first, second))
import GhcLib.Transform.Utility (varAppearsInExpr)

etaReduce :: GHC.CoreProgram -> GHC.CoreProgram

-- | eta reduction, e.g., \x -> f x => f
etaReduce =  rewriteBi etaRed
  where
    etaRed :: GHC.CoreExpr -> Maybe GHC.CoreExpr
    etaRed (GHC.Lam v (GHC.Tick t (GHC.App f args))) | isEtaReducible f args v = return (GHC.Tick t f)
    etaRed (GHC.Lam v (GHC.App (GHC.Tick t f) args)) | isEtaReducible f args v = return (GHC.Tick t f)
    etaRed (GHC.Lam v (GHC.App f args)) | isEtaReducible f args v = return f
    etaRed (GHC.Lam v (GHC.Let b (GHC.Tick t (GHC.App f args)))) | isEtaReducible f args v = return (GHC.Let b (GHC.Tick t f))
    etaRed (GHC.Lam v (GHC.Let b (GHC.App f args))) | isEtaReducible f args v = return (GHC.Let b f)
    etaRed _ = Nothing

isEtaReducible :: GHC.CoreExpr -> GHC.CoreExpr -> GHC.Var -> Bool
isEtaReducible f arg v = case arg of
    GHC.Var v'
        | v == v'
        , not (GHC.isTyVar v)
        , not (GHC.isLinearType (GHC.exprType f)) -- don't eta-reduce eta-expanded data constructors (with linear types)
        , not (varAppearsInExpr v f)                   -- don't eta reduce if variable used somewhere else in the expression
        , not (GHC.isEvVar v) ->              -- don't remove evidence variables
                            True
    GHC.Tick _ e -> isEtaReducible f e v
    _ -> False
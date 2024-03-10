
{-# OPTIONS_GHC -Wno-orphans #-}
module GhcLib.Utility.BinderEquality where

import qualified GHC
import GHC.Plugins qualified as GHC

import GhcLib.Utility.Bag ()

-- | A variant of getModSummary presented in the GHC API documentation.
import qualified GHC.Core.Map.Type as GHC


instance Eq (GHC.DeBruijn GHC.Var) where
  (==) :: GHC.DeBruijn GHC.Var -> GHC.DeBruijn GHC.Var -> Bool
  (==) = eqDeBruijnVar

eqDeBruijnVar :: GHC.DeBruijn GHC.Var -> GHC.DeBruijn GHC.Var -> Bool
eqDeBruijnVar (GHC.D env1 v1) (GHC.D env2 v2) =
    case (GHC.lookupCME env1 v1, GHC.lookupCME env2 v2) of
        (Just b1, Just b2) -> b1 == b2
        (Nothing, Nothing) -> v1 == v2
        _ -> False

instance Eq (GHC.DeBruijn (GHC.Bind GHC.CoreBndr)) where
    (==) :: GHC.DeBruijn (GHC.Bind GHC.CoreBndr) -> GHC.DeBruijn (GHC.Bind GHC.CoreBndr) -> Bool
    (==) (GHC.D env1 e1) (GHC.D env2 e2) = go e1 e2 where
      go :: GHC.Bind GHC.CoreBndr -> GHC.Bind GHC.CoreBndr -> Bool
      go (GHC.NonRec b1 e1) (GHC.NonRec b2 e2) = eqDeBruijnVar (GHC.D env1 b1) (GHC.D env2 b2)
                                                      && (GHC.D env1 e1) == (GHC.D env2 e2)
      go (GHC.Rec bs1) (GHC.Rec bs2) = and $
            zipWith (\(b1,e1) (b2,e2) -> eqDeBruijnVar (GHC.D env1 b1) (GHC.D env2 b2) && GHC.D env1 e1 == GHC.D env1 e2 ) bs1 bs2
      go _ _ = False

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
module GhcLib.Transform.Rename where 

import qualified GHC.Core.Utils as GHC 
import qualified GHC.Core as GHC
import qualified GHC.Types.Var as GHC 
import qualified GHC.Types.Id as GHC 
import qualified GHC.Types.Unique.Supply as GHC 
import qualified GHC.Core.Type as GHC 
import qualified GHC.Core.Predicate as GHC 
import qualified GHC.Types.Unique as GHC
import qualified GHC.Types.Id.Info as GHC 
import qualified GHC.Core.Opt.Arity as GHC 
import qualified GHC.Types.Name as GHCOcc 

import Control.Monad.State
import Data.Generics.Uniplate.Data 
import qualified Data.Map as Map 
import Data.Maybe (fromJust)

import GhcLib.Transform.Utility




-- alpha renaming and other replacing transformations
------------------------------------------------------
replaceHoles :: GHC.UniqSupply -> GHC.CoreProgram -> GHC.CoreProgram
-- | Replace holes (case typerror) with variables
replaceHoles holeIdentSupply p =  evalState (repHoles p) $ HoleCandidates 0 $ GHC.listSplitUniqSupply holeIdentSupply
    where repHoles :: GHC.CoreProgram -> State HoleCandidates GHC.CoreProgram
          repHoles = transformBiM $ \case
            c@(GHC.Case e v t [])
                | isHoleExpr c -> do
                                holeIdCandidates <- gets holeNameCandicate
                                holeCount <- gets holeCount
                                modify $ 
                                    \s -> s { holeCount = holeCount + 1, holeNameCandicate = tail holeIdCandidates }      
                                let -- id = fromJust (getTypErr e)
                                    -- ? TODO : check the result of typ and t are the same 
                                    typ = GHC.exprType c -- might be another type
                                    uq = GHC.uniqFromSupply (head holeIdCandidates)
                                    name = makeName ("hole_" ++ show holeCount) uq (GHCOcc.getSrcSpan (GHC.varName v)) 
                                    id' = GHC.setIdInfo (GHC.setVarType (GHC.setVarName v name) typ) idinf
                                    idinf = GHC.setArityInfo GHC.vanillaIdInfo (GHC.exprArity e)
                                return $ GHC.Var (GHC.globaliseId id') -- make global Id since hole could be something from any scope 
                | otherwise -> return c
            e -> return e


replacePatErrors :: GHC.CoreProgram -> GHC.CoreProgram
-- | Replace pattern errors (case patError) with variables
replacePatErrors = repPatErr
    where repPatErr :: GHC.CoreProgram -> GHC.CoreProgram
          repPatErr = transformBi $ \case
            c@(GHC.Case e v t []) -- we only replace paterrors with empty alternatives 
                | isPatError c -> let id = fromJust (getPatErr e)
                                      ty = GHC.exprType c -- check this type
                                      name = makeName "patError" (GHC.getUnique id) (GHCOcc.getSrcSpan (GHC.varName v))
                                      id' = GHC.setIdInfo (GHC.setVarType (GHC.setVarName v name) ty) idinf
                                      idinf = GHC.setArityInfo GHC.vanillaIdInfo (GHC.exprArity e)
                                   in GHC.Var (GHC.globaliseId id') 
                | otherwise -> c
            e -> e

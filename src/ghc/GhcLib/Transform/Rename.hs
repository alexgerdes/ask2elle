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
import Debug.Trace (trace)




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

-- replacing case binders with scrutinee
replaceCaseBinds :: GHC.CoreProgram -> GHC.CoreProgram
-- | Substitute back the scrutinee for case binded name in case expressions
replaceCaseBinds = transformBi repBinds
    where repBinds :: GHC.CoreExpr -> GHC.CoreExpr
          -- | replace case result binder with scrutinee (if the case binder is "wild")
          --  e.g. Case xs of wild -> {_ -> f wild} =>  Case xs of wild -> {_ -> f xs}
          repBinds (GHC.Case scrutinee wild t as) | isWild wild = GHC.Case scrutinee wild t (map (sub scrutinee wild) as)
              where sub :: GHC.CoreExpr ->  GHC.Var -> GHC.Alt GHC.Var -> GHC.Alt GHC.Var
                    sub e v (GHC.Alt ac vars ex) = GHC.Alt ac vars (subsExpr e v ex)
          repBinds e = e


-- 


-- variable renaming
data VarType = TopBind | CaseBind | GenVar | LamVar

data St = St {
         env  :: Map.Map GHC.Var GHC.Var
        ,topBindCount      :: Int -- to rename all top-level binders separately 
        ,freshCaseBindVar  :: Int
        ,freshVar          :: Int
        ,freshBindVar      :: Int
        ,freshLamVar       :: Int
        ,exerName  :: String
        }

type Ctx a = State St a


initSt :: St
initSt = St {env = Map.empty, topBindCount = 0, freshCaseBindVar = 0, freshVar = 0, freshBindVar = 0, freshLamVar = 0, exerName = ""}

alpha :: String -> GHC.CoreProgram ->  (GHC.CoreProgram, Map.Map GHC.Var GHC.Var)
-- | Do renaming and return map of renamed variables        
alpha fname cs = (prog, env state)
    where (prog,state) = runState st (initSt {exerName = fname})
          st = mapM alphaR cs
          alphaR cb = modify resetState >> alphaB cb

resetState :: St -> St
-- Reset name counters
resetState st = st {topBindCount = topBindCount st + 1,
                    freshCaseBindVar = 0,
                    freshVar = 0,
                    freshBindVar = 0}


alphaB :: GHC.CoreBind -> Ctx GHC.CoreBind
alphaB cb = do
    alpha cb
    where alpha = \case
            (GHC.NonRec v e) -> do
                v' <- renameVar TopBind v
                GHC.NonRec v' <$> aRename e
            (GHC.Rec es) -> do
                vars <- mapM (renameVar TopBind . fst) es
                exps <- mapM (aRename . snd) es
                return $ GHC.Rec (zip vars exps)

renameVar :: VarType -> GHC.Id -> Ctx GHC.Id
renameVar vty v | isSpecialVar = return v
                | otherwise = do
            env <- gets env
            name <- gets exerName
            if GHCOcc.getOccString v == name -- ! this differs from Matilda code, check if something went wrong
                then return v 
                else case Map.lookup v env of
                    Just n  | GHC.varType n `GHC.eqType` GHC.varType v -> return n
                            | otherwise -> renameV vty v 
                    Nothing -> renameV vty v 
    where isSpecialVar = 
                 GHC.isGlobalId v -- globalIds are imported, or data constructors, or primops, or record selectors
              || GHC.isTyCoVar v  -- is a type or coercion variable
              || isSpecVar v  -- does it leads with a $ sign?
              || GHC.isImplicitId v -- is it a foreign function call or a data constructor or a data constructor wrapper, or a type class function?
              || GHC.isTcTyVar v   -- is it a typechecking type variable
              || GHC.isJoinId v    -- is this a join point variable?
              || GHC.isEvVar v     -- is a type level variable ?
              || GHC.isConLikeId v -- is it a variable relates to inlining?


renameV :: VarType -> GHC.Var -> Ctx GHC.Var
renameV vty v = do
    vname <- getVName vty
    let v' = GHC.setVarName v (makeName vname (GHC.getUnique v) (GHCOcc.getSrcSpan (GHC.varName v)))
    modify $ \s -> s {env = Map.insert v v' (env s)} -- update map 
    return v'

getVName :: VarType -> Ctx String
getVName vty = do
    bc <- gets topBindCount
    case vty of
        CaseBind -> do
                i <- gets freshCaseBindVar
                modify $ \ s -> s {freshCaseBindVar = i+1}
                return ("cb"++show bc ++ show i)
        TopBind -> do
                i <- gets freshBindVar
                modify $ \ s -> s {freshBindVar = i+1}
                return ("b" ++ show bc ++ show i)
        GenVar -> do
                i <- gets freshVar
                modify $ \ s -> s {freshVar = i+1}
                return ("v" ++ show bc ++ show i)
        LamVar -> do
                i <- gets freshLamVar
                modify $ \ s -> s {freshLamVar = i+1}
                return ("l" ++ show bc ++ show i)

aRename :: GHC.CoreExpr -> Ctx GHC.CoreExpr
aRename v@(GHC.Var id)     = GHC.Var <$> renameVar GenVar id
aRename t@(GHC.Type _)     = return t
aRename l@(GHC.Lit _)      = return l
aRename (GHC.App e arg)    = do
        e' <- aRename e
        arg' <- aRename arg
        return $ GHC.App e' arg'
aRename l@(GHC.Lam b e)      = do --  trace ("Lambda var:" ++ show b ++ " isTyVar: " ++ show (isTyVar b)) $
        b' <- renameVar LamVar b
        e' <- aRename e
        return $ GHC.Lam b' e'
aRename c@(GHC.Case e v t a) = do
                            e' <- aRename e
                            v' <- renameVar CaseBind v
                            a' <- renameAlt a
                            return $ GHC.Case e' v' t a'
aRename (GHC.Cast e co)    =
                         do
        e' <- aRename e
        return $ GHC.Cast e' co
aRename (GHC.Let b e)      = do
    b' <- alphaB b
    e' <- aRename e
    return $ GHC.Let b' e'
aRename (GHC.Tick ct e)    = aRename e >>= \e' -> return (GHC.Tick ct e')
aRename x              = return x

renameAlt :: [GHC.Alt GHC.Var] -> Ctx [GHC.Alt GHC.Var]
renameAlt = mapM renameAlt'
    where renameAlt' :: GHC.Alt GHC.Var -> Ctx (GHC.Alt GHC.Var)
          renameAlt' (GHC.Alt ac vs e) = do
            vs' <- mapM (renameVar GenVar) vs
            e' <- aRename e
            return $ GHC.Alt ac vs' e'
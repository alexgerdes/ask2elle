module GhcLib.Transform.Transform  (preProcess) where
-- module GhcLib.Transform.Transform  (normalise, preProcess, removeTyEvidence, alpha) where


import qualified GHC.Core as GHC 
import qualified GHC.Types.Var as GHC
import Data.Map (Map)

import GhcLib.Transform.Rename (replaceHoles, replacePatErrors)
import qualified GHC.Types.Unique.Supply as GHC
-- import Utils.Utils 
-- import Transform.Eta ( etaReduce ) 
-- import Transform.Inline ( inlineBinds, recToLetRec ) 
-- import Transform.Remove ( removeRedEqCheck, removeTyEvidence) 
-- import Transform.Rename
--     ( alpha, 
--     replaceCaseBinds, 
--     replaceHoles, 
--     replacePatErrors ) 


preProcess :: GHC.UniqSupply -> GHC.CoreProgram -> GHC.CoreProgram
-- | Preprocessing transformations
preProcess identSupply p = replacePatErrors $ replaceHoles identSupply p 

-- normalise :: String -> CoreProgram -> IO (CoreProgram, Map Var Var)
-- -- | Normalising transformations
-- normalise name p = inlineBinds name p >>=
--                    recToLetRec >>=
--                    removeRedEqCheck >>=
--                    replaceCaseBinds >>=
--                    etaReduce >>=
--                    alpha name



--- EXPERIMENTAL STUFF BELOW
----------------------------------------------------

{- inlineRedLets :: CoreProgram -> IO CoreProgram
-- | Inline redundant let expressions, e.g. let f = x in g f => g x 
inlineRedLets = return . rewriteBi removeLet

removeLet :: CoreExpr ->  Maybe CoreExpr
removeLet (Let (NonRec b ex) e) | isTyApplication ex && isVar innExp = Just $ subst (getVar innExp) b e
    where innExp = getInnerExp ex
          getVar (Var v) = v
removeLet _ = Nothing

isTyApplication :: CoreExpr -> Bool
isTyApplication (App e v) | isEvOrTyExp v = True
                          | otherwise = isTyApplication e

getInnerExp :: CoreExpr -> CoreExpr
getInnerExp (App e v) | isEvOrTyExp v = getInnerExp e
                      | otherwise     = App e v
getInnerExp e = e


floatOutLets :: CoreProgram -> CoreProgram
-- | Float let-binders to toplevel, e.g. f = let g = x in g => x 
floatOutLets = transformBi $ \bind -> case bind :: CoreBind of
     (NonRec v (Lam a (Let b (App (Var v') (Var a'))))) | getBindTopVar b ~= v'
                                                        , a ~= a' -> transformBi (subst v v') (setBindTopVar v b)
     (NonRec v (Let b (Var v'))) | getBindTopVar b == v' -> transformBi (subst v v') (setBindTopVar v b)

     bi -> bi

setBindTopVar :: Var -> CoreBind -> CoreBind
setBindTopVar new (NonRec v e)     = NonRec new e
setBindTopVar new (Rec ((v,e):es)) = Rec ((new,e):es)


floatOut :: CoreProgram -> Ghc CoreProgram
-- | Using the float out transformation from GHC
floatOut p = do
    df <- getSessionDynFlags
    logger <- liftIO initLogger
    let floatSw = FloatOutSwitches {
            floatOutLambdas = Just 1,    -- float all lambdas to top level,
            floatOutConstants = False,    -- True => float constants to top level,
            floatOutOverSatApps = False,   -- True => float out over-saturated application
            floatToTopLevelOnly = True    -- Allow floating to the top level only.
            }
    us <- liftIO $ mkSplitUniqSupply 'z'
    liftIO $ floatOutwards logger floatSw df us p -}


{- addDefaultCase :: CoreProgram -> Ghc CoreProgram
addDefaultCase p = do
    env <- getSession
    let ic = hsc_IC env
    let inscopeVars = mkInScopeSet $ mkUniqSet $ interactiveInScope ic
    transformBiM (addDefCase inscopeVars) p

newGhcVar :: Type -> InScopeSet -> Ghc Id
newGhcVar t is = do
    let uq = unsafeGetFreshLocalUnique is
        name = makeName "fresh" uq (mkGeneralSrcSpan (mkFastString "Dummy location"))
        id'  = mkLocalId name t t  -- reuse id information from top-level binder'
    return id'

addDefCase :: InScopeSet -> CoreBind -> Ghc CoreBind
addDefCase is (NonRec b e)     | not (hasCase e) && not (isEvOrTyVar b) = (addCase is e) >>= \ex -> return $ NonRec b ex
addDefCase is (Rec ((b,e):es)) | not (hasCase e) && not (isEvOrTyVar b) = (addCase is e) >>= \ex -> return $ Rec ((b,ex):es)
addDefCase is b = return b
         -}                -- | needsCaseBinding (varType v) e = addCase e v -- rather tests whether
                                                                       -- needs to use a case rather than let bind

{- addCase :: InScopeSet -> Expr Var -> Ghc (Expr Var)
addCase is e = do
    let v = getFirstNonTypeLam e
    let t = ft_res $ ft_res (dropForAlls $ exprType e)
    wild <- newGhcVar t is  -- should have same type as the case 
    return $ liftLambdas e (Case (Var v) wild t [Alt DEFAULT [] (innerExp e)])
    where innerExp (Lam v e) = innerExp e
          innerExp e         = e
          liftLambdas (Lam v e) ex = Lam v (liftLambdas e ex)
          liftLambdas _ ex         = ex
          getFirstNonTypeLam (Lam v e) | isEvOrTyVar v = getFirstNonTypeLam e
                                       | otherwise = v
          getFirstNonTypeLam ex = error (show ex) -}

{- removeTyErrors :: CoreProgram -> IO CoreProgram 
-- | Remove non-hole type errors
removeTyErrors p = return . rewriteBi remTyErr $ p 
    where remTyErr (App ex c@(Case {})) | isTyError c = return ex 
          remTyErr (App c@(Case {}) ex) | isTyError c = return ex 
          remTyErr _ = Nothing 
 -}
{- addDefaultCase :: CoreProgram -> CoreProgram
addDefaultCase p = evalState (pm p) initSt
    where pm :: CoreProgram -> Ctx CoreProgram
          pm = transformBiM addDefCase
addDefCase :: Expr Var -> Ctx (Expr Var)
addDefCase ex@(Lam v e) | isTyVar v                     = Lam v <$> addDefCase e
                        | needsCaseBinding (varType v) e = addCase e v -- rather tests whether
                                                                       -- needs to use a case rather than let bind
addDefCase e = return e -}
{- 
addCase :: Expr Var -> Var -> Ctx (Expr Var)
addCase e v = do
    let t = varType v
    fresh <- freshVar t
    wild <- freshVar t  -- should have same type as the case 
    e' <- subst_ fresh v e
    return $ Lam fresh $ mkDefaultCase (Var fresh) wild e'
 -}

-- Core Utils 
-- mkSingleAltCase
-- needsCaseBInding
-- bindNonRec
-- mkAltExpr -- make case alternatives 
-- | Extract the default case alternative
-- findDefault :: [Alt b] -> ([Alt b], Maybe (Expr b))
-- -- | Find the case alternative corresponding to a particular
-- constructor: panics if no such constructor exists
-- findAlt :: AltCon -> [Alt b] -> Maybe (Alt b)
-- check if we can use diffBinds from Core.Utils to find small diffs 
-- diffExpr instead of my similarity relation? need an RnEnv2
-- mkLamTypes :: [Var] -> Type -> Type
-- can this be used for beta-expansioN???????
-- applyTypeToArgs :: HasDebugCallStack => SDoc -> Type -> [CoreExpr] -> Type
-- ^ Determines the type resulting from applying an expression with given type
--- to given argument expressions.
-- Do I need to do this backwards when eta-reducing?
{- caseToGuard :: BiplateFor CoreProgram => CoreProgram -> CoreProgram
caseToGuard = rewriteBi etaRed
ctg :: Expr Var ->  Maybe (Expr Var)
-- | case to guard, e.g. case e of {Just a -> a} => case (e == Just a) of {True -> a}
ctg (Lam v (App f args)) =
   case args of
      Var v' | v == v' -> return f
      _                -> Nothing
ctg _ = Nothing

-}
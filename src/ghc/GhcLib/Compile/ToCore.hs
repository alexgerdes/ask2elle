{-# LANGUAGE RankNTypes #-}
module GhcLib.Compile.ToCore  where

import GHC qualified
import GHC.Data.Bag qualified as GHC
import GHC.Data.EnumSet qualified as GHCEnumSet
import GHC.Driver.Monad qualified as GHC
import GHC.Driver.Session qualified as GHC
import GHC.Driver.Main qualified as GHC
import GHC.Driver.Make qualified as GHC
import GHC.LanguageExtensions.Type qualified as GHC
import GHC.Unit.Module.Graph qualified as GHC
import GHC.Plugins qualified as GHC
import GHC.Types.Error qualified as GHC
import GHC.Data.Graph.Directed qualified as GHC
import GHC.Utils.Logger qualified as GHCLogger
import GHC.Utils.Error qualified as GHCUtils
import GHC.Unit.Home.ModInfo qualified as GHC
import GHC.Linker.Types qualified as GHC
import GHC.Runtime.Interpreter qualified as GHC
import GHC.Conc qualified as GHC
import GHC.Unit.Module.Graph qualified as GHC
import GHC.Utils.Ppr qualified as GHC
import GHC.Plugins qualified as GHC 
import GHC.Core.Opt.Pipeline qualified as GHC
import GHC.Data.StringBuffer qualified as GHC
import GHC.Unit.Module.Name qualified as GHC 

import Control.Monad.Catch
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Exception (evaluate)
import Control.Monad.Reader
import Data.Either
import Data.IORef (IORef, newIORef)
import Data.Maybe
import Data.Map qualified as Map
import Data.String (IsString (fromString))
import System.FilePath (takeBaseName)
import System.Process (readProcess)
import System.IO (stdout)
import Data.Time.Clock

import GhcLib.GHCRelated.Bag ()
import GhcLib.Utility.Flags
import GhcLib.GHCRelated.Utility
import GhcLib.GHCRelated.Warning
import GhcLib.Transform.Transform
import GhcLib.GHCRelated.ShowCore
import Control.Monad.RWS (MonadState(put))
import Debug.Trace (traceM)
import GhcLib.Transform.Inline (recToLetRec)
import GhcLib.Transform.Remove (removeTyEvidence)
import GHC.Base (build)


data ToCoreOption = ToCoreOption
    { getStudentSolution :: GHC.StringBuffer 
    , getStudentExerciseName :: String
    }
    deriving stock (Show)

data ToCoreOutput = ToCoreOutput 
    { resultCoreProgram :: GHC.CoreProgram
    , parsedModule :: GHC.ParsedSource
    , alphaRenamingMapping :: Map.Map GHC.Var GHC.Var
    , studentExerciseName :: String}

    
data ToCoreError = FailedUnloading | FailedLoading | NotInsideModuleGraph String | ParsingError GHC.ErrorMessages | TypecheckingError GHC.ErrorMessages 
    deriving stock (Show)


instance Exception ToCoreError

-- newtype ToCore a = MkToCore {runToCore :: GHC.GhcT (ExceptT ToCoreError IO) a}
--     deriving newtype (Functor, Applicative, Monad, MonadIO,GHC.HasDynFlags, GHCLogger.HasLogger)
--     deriving (MonadThrow, MonadCatch, MonadMask) via (ReaderT GHC.Session (ExceptT ToCoreError IO))
--     deriving newtype (GHC.GhcMonad)

-- newtype Wrapper a = MkWrapper {runWrapper :: ReaderT ToCoreOption ToCore a }

newtype ToCore a = MkToCore {runToCore :: GHC.GhcT (ReaderT ToCoreOption (ExceptT ToCoreError IO)) a}
    deriving newtype (Functor, Applicative, Monad, MonadIO, GHC.HasDynFlags, GHCLogger.HasLogger)
    deriving (MonadThrow, MonadCatch, MonadMask) via (ReaderT GHC.Session (ReaderT ToCoreOption (ExceptT ToCoreError IO)))
    deriving newtype (GHC.GhcMonad)

setExtensionFlag' :: GHC.Extension -> GHC.DynFlags -> GHC.DynFlags

-- |
-- Reimplementation of setExtensionFlag' as ghc doesn't export it
--
-- When you set f, set the ones it implies
--
--  NB: use setExtensionFlag recursively, in case the implied flags
--      implies further flags
setExtensionFlag' f dflags = foldr ($) (GHC.xopt_set dflags f) deps
  where
    deps =
        [ if turn_on
            then setExtensionFlag' d
            else unSetExtensionFlag' d
        | (f', turn_on, d) <- GHC.impliedXFlags
        , f' == f
        ]

unSetExtensionFlag' :: GHC.Extension -> GHC.DynFlags -> GHC.DynFlags

-- | Reimplementation of unSetExtensionFlag' as ghc doesn't export it
--
-- When you un-set f, however, we don't un-set the things it implies
--
--      (except for -fno-glasgow-exts, which is treated specially)
unSetExtensionFlag' f dflags = GHC.xopt_unset dflags f

opt_set :: (GHC.DynFlags -> a -> GHC.DynFlags) -> [a] -> GHC.DynFlags -> GHC.DynFlags
opt_set f flags df = Prelude.foldl f df flags

eopt_unset :: GHC.DynFlags -> GHC.Extension -> GHC.DynFlags
eopt_unset df ext = df{GHC.extensionFlags = GHCEnumSet.delete ext (GHC.extensionFlags df)}

eopt_set :: GHC.DynFlags -> GHC.Extension -> GHC.DynFlags
eopt_set df ext = df{GHC.extensionFlags = GHCEnumSet.insert ext (GHC.extensionFlags df)}


setFlags :: Bool -> [GHC.GeneralFlag] -> ToCore ()

-- | Set extra dynamic flags(flags from the command line) along with default always-on flags
setFlags b flags = do
    currDynFlags <- GHC.getSessionDynFlags
    -- let existingGenFlags = GHC.generalFlags currDynFlags
    -- liftIO $ putStrLn "General Flags : "
    -- mapM_ (\flag -> liftIO $ print $ show flag) $ GHCEnumSet.toList existingGenFlags
    -- let existingWarnFlags = GHC.warningFlags currDynFlags
    -- liftIO $ putStrLn "Warning Flags : "
    -- mapM_ (\flag -> liftIO $ print $ show flag) $ GHCEnumSet.toList existingWarnFlags
    -- let existingFatalFlags = GHC.fatalWarningFlags currDynFlags
    -- liftIO $ putStrLn "Fatal Warning Flags : "
    -- mapM_ (\flag -> liftIO $ print $ show flag) $ GHCEnumSet.toList existingFatalFlags
    let gflags = if b then GHCEnumSet.toList (GHC.generalFlags currDynFlags) ++ flags else flags
        -- dflags =
        --     ( opt_set gopt_unset unsetGenFlags
        --         . opt_set wopt_unset unsetWarnFlags
        --         . opt_set wopt_set setWarnFlags
        --         . opt_set eopt_set extFlags
        --         . opt_set eopt_unset unsetExtFlags
        --         . opt_set gopt_set gflags
        --     )
        --     df
        disableGeneralFlag = foldl (flip GHC.unSetGeneralFlag') currDynFlags unsetGenFlags
        suppressWarningFlag = foldl GHC.wopt_unset disableGeneralFlag unsetWarnFlags
        enableWarningFlag = foldl GHC.wopt_set suppressWarningFlag setWarnFlags
        enableGhcExtension = foldl (flip setExtensionFlag') enableWarningFlag extFlags
        disableGhcExtension = foldl (flip unSetExtensionFlag') enableGhcExtension unsetExtFlags
        enableGeneralFlag = foldl (flip GHC.setGeneralFlag') disableGhcExtension gflags
    {- to update DynFlags directly
    dflags' = dflags {refLevelHoleFits = Just 2,
                      maxValidHoleFits = Just 8,
                      maxRefHoleFits   = Just 10}
    -}

    GHC.setSessionDynFlags enableGeneralFlag
    -- let currGenFlags = GHC.generalFlags currDynFlags
    -- liftIO $ putStrLn "General Flags : "
    -- mapM_ (\flag -> liftIO $ print $ show flag) $ GHCEnumSet.toList $ GHCEnumSet.difference currGenFlags existingGenFlags
    -- let currWarnFlags = GHC.warningFlags currDynFlags
    -- liftIO $ putStrLn "Warning Flags : "
    -- mapM_ (\flag -> liftIO $ print $ show flag) $ GHCEnumSet.toList $ GHCEnumSet.difference currWarnFlags existingWarnFlags
    -- let currFatalFlags = GHC.fatalWarningFlags currDynFlags
    -- liftIO $ putStrLn "Fatal Warning Flags : "
    -- mapM_ (\flag -> liftIO $ print $ show flag) $ GHCEnumSet.toList $ GHCEnumSet.difference currFatalFlags existingFatalFlags

-- | ghc loads the target file
loadWithoutPlugins :: GHC.Target -> ToCore ()
loadWithoutPlugins targetFile = do
    -- first unload all the files (like GHCi :load does)
    GHC.setTargets []
    unloadingFlag <- GHC.load GHC.LoadAllTargets
    guardS (GHC.succeeded unloadingFlag) "Error : Clearing context failed" FailedUnloading
    --  Why do we need to set this option to Nothing?\
    -- traceM $ "Trace point : " ++ (show $ GHC.outputFile_ dflags)
    -- Nothing
    -- GHC.setSessionDynFlags dflags{GHC.outputFile_ = Nothing}
    GHC.setTargets [targetFile]
    hsFile <- liftToCore $ asks getStudentExerciseName
    -- * The following only perform dependency analysis 
    maybeLoaded <- GHC.handleSourceError (pure . Left . GHC.srcErrorMessages) (Right <$>  GHC.depanalE [] False   )
    guardS (isRight maybeLoaded) (fromString $ "Error : Failed to parse " ++ hsFile) (ParsingError $ fromLeft GHC.emptyBag maybeLoaded)
    -- guardS (GHC.succeeded $ fromRight GHC.Succeeded maybeLoaded) (fromString $ "Error : Failed to load target : " ++ targetFP) FailedLoading
-- >>> take 10 [x | x <- [1..], odd x]


-- customizedLoad' :: Maybe GHC.Messager -> GHC.ModuleGraph -> ToCore ()
-- customizedLoad' mHscMessage mod_graph = do
--     GHC.modifySession $ \hsc_env -> hsc_env {GHC.hsc_mod_graph = mod_graph}
--     -- GHC.guessOutputFile -- ? not sure we need this function
--     hsc_env <- GHC.getSession 

--     let hpt1 = GHC.hsc_HPT hsc_env 
--         dflags = GHC.hsc_dflags hsc_env 
--         logger = GHC.hsc_logger hsc_env
--         interp = GHC.hscInterp hsc_env

--     let all_home_mods =
--           -- take all modules which are derived from hs-boot files
--           GHC.mkUniqSet [ GHC.ms_mod_name s
--                     | s <- GHC.mgModSummaries mod_graph, GHC.isBootSummary s == GHC.NotBoot]


--     let mg2_with_srcimps :: [GHC.SCC GHC.ModSummary]
--         mg2_with_srcimps = GHC.filterToposortToModules $
--                     GHC.topSortModuleGraph True mod_graph Nothing

--     -- If we can determine that any of the {-# SOURCE #-} imports
--     -- are definitely unnecessary, then emit a warning.
--     GHC.warnUnnecessarySourceImports mg2_with_srcimps

--     let
--         -- check the stability property for each module.
--         stable_mods@(stable_obj,stable_bco)
--             = GHC.checkStability hpt1 mg2_with_srcimps all_home_mods

--         pruned_hpt = hpt1

--     _ <- liftIO $ evaluate pruned_hpt

--     -- before we unload anything, make sure we don't leavedebugTraceMsg  an old
--     -- interactive context around pointing to dead bindings.  Also,
--     -- write the pruned HPT to allow the old HPT to be GC'd.
--     GHC.setSession $ GHC.discardIC $ hsc_env { GHC.hsc_HPT = pruned_hpt }

--     let msg = GHC.text "Stable obj:" GHC.<+> GHC.ppr stable_obj GHC.$$
--                             GHC.text "Stable BCO:" GHC.<+> GHC.ppr stable_bco
--     liftIO $ GHCUtils.debugTraceMsg logger dflags 2 msg

--     let stable_linkables = [ linkable
--                         | m <- GHC.nonDetEltsUniqSet stable_obj ++
--                                 GHC.nonDetEltsUniqSet stable_bco,
--                             -- It's OK to use nonDetEltsUniqSet here
--                             -- because it only affects linking. Besides
--                             -- this list only serves as a poor man's set.
--                             Just hmi <- [GHC.lookupHpt pruned_hpt m],
--                             Just linkable <- [GHC.hm_linkable hmi] ]
--     liftIO $ GHC.unload interp hsc_env stable_linkables

--     let full_mg, partial_mg0, partial_mg, unstable_mg :: [GHC.SCC GHC.ModuleGraphNode]
--         stable_mg :: [GHC.SCC GHC.ExtendedModSummary]
--         full_mg  = GHC.topSortModuleGraph False mod_graph Nothing

--         maybe_top_mod = Nothing
--         -- LoadDependenciesOf m: we want the upsweep to stop just
--         -- short of the specified module (unless the specified module
--         -- is stable).
--         partial_mg0 = GHC.topSortModuleGraph False mod_graph maybe_top_mod
--         partial_mg = partial_mg0

--         stable_mg = [ GHC.AcyclicSCC ems
--             | GHC.AcyclicSCC (GHC.ModuleNode ems@(GHC.ExtendedModSummary ms _)) <- full_mg
--             , stable_mod_summary ms
--             ]
--         stable_mod_summary ms =
--             GHC.ms_mod_name ms `GHC.elementOfUniqSet` stable_obj ||
--             GHC.ms_mod_name ms `GHC.elementOfUniqSet` stable_bco

--         -- the modules from partial_mg that are not also stable
--         -- NB. also keep cycles, we need to emit an error message later
--         unstable_mg = filter not_stable partial_mg
--           where not_stable (GHC.CyclicSCC _) = True
--                 not_stable (GHC.AcyclicSCC (GHC.InstantiationNode _)) = True
--                 not_stable (GHC.AcyclicSCC (GHC.ModuleNode (GHC.ExtendedModSummary ms _)))
--                    = not $ stable_mod_summary ms
--         -- Load all the stable modules first, before attempting to load
--         -- an unstable module (#7231).
--         mg = fmap (fmap GHC.ModuleNode) stable_mg ++ unstable_mg

--     liftIO $ GHCUtils.debugTraceMsg logger dflags 2 (GHC.hang (GHC.text "Ready for upsweep")
--                                2 (GHC.ppr mg))

--     n_jobs <- case GHC.parMakeCount dflags of
--                     Nothing -> liftIO GHC.getNumProcessors
--                     Just n  -> return n

--     let upsweep_fn | n_jobs > 1 = parUpsweep n_jobs
--                    | otherwise  = upsweep
--     pure ()




initEnv :: Bool -> [GHC.GeneralFlag] -> ToCore (IORef [Warning])
initEnv keepDefaultFlags flags = do
    solution <- liftToCore $ asks getStudentSolution
    exerciseName <- liftToCore $ asks getStudentExerciseName
    setFlags keepDefaultFlags flags
    -- logger <- GHC.getLogger
    -- ! Not sure whether we need this in the future
    ref <- liftIO (newIORef [])
    -- in case of logging, also write it to the IORef
    GHC.pushLogHookM (writeWarnings ref)
    -- target <- GHC.guessTarget hsFilePath Nothing
    -- (Target (TargetFile str (Just phase)) True Nothing)
    complieTime <- liftIO  getCurrentTime
    let target = GHC.Target (GHC.TargetFile "NeverExistedLocalFile.hs" Nothing) True $ Just (solution,complieTime)
    loadWithoutPlugins target
    return ref


desugarToCore :: Bool -> [GHC.GeneralFlag] -> ToCore (GHC.ModGuts, GHC.ParsedSource)
-- | Compile a haskell file to the desugar pass + simple optimiser and return Modguts and warnings
desugarToCore keepExistingFlags flags = do
    hsFile <- liftToCore $ asks getStudentExerciseName
    ref <- initEnv keepExistingFlags flags
    -- * Check target exists in the module graph
    modSum <- getMaybeSModSummary $ GHC.mkModuleName hsFile
    guardS (isJust modSum) (fromString $ "Error : " ++ hsFile ++ " is not part of the module graph") $ NotInsideModuleGraph hsFile
    -- * If target exists in the module graph, it should have already been a well-parsed mod
    eitherParsed <- GHC.handleSourceError (pure . Left . GHC.srcErrorMessages) (Right <$>  GHC.parseModule (fromJust modSum))
    guardS (isRight eitherParsed) (fromString $ "Error : Failed to parse " ++ hsFile) (ParsingError $ fromLeft GHC.emptyBag eitherParsed)
    -- * Checking if the module is well-typed
    let safeParseResult = fromRight (error "Impossible : This shoud be a Right parsed module value, but received a Left") eitherParsed
    eitherTyped <- GHC.handleSourceError (pure . Left . GHC.srcErrorMessages) (Right <$>  GHC.typecheckModule safeParseResult)
    guardS (isRight eitherTyped) (fromString $ "Error : Failed to type check " ++ hsFile) (TypecheckingError $ fromLeft GHC.emptyBag eitherTyped)
    -- * Desugaring the typechecked module
    let typecheckedResult = fromRight (error "Impossible : shoud be a Right well-typed value, but received a Left") eitherTyped
    desugaredMod <- GHC.desugarModule typecheckedResult
    let coreMod = GHC.dm_core_module desugaredMod
    -- -- * Print the core module
    -- liftIO $ putStrLn "Core Module : "
    -- liftIO $ putStrLn $ show $ GHC.mg_binds coreMod
    -- liftIO $ putStrLn "\n------------------------------------"
    -- liftIO $ GHC.printSDoc GHC.defaultSDocContext GHC.ZigZagMode stdout (GHC.ppr $ GHC.mg_binds coreMod)
    -- liftIO $ putStrLn "\n------------------------------------"
    return (coreMod, GHC.pm_parsed_source safeParseResult)


desugarPreprocess :: ToCore (GHC.CoreProgram, GHC.ParsedSource)
-- |  a haskell file -> desugar pass(including simple optimiser) -> handling type hole error
desugarPreprocess = do
    (mgCore, parsedSourceCode) <- desugarToCore False (holeFlags ++ genFlags)
    uniqHoleSupply <- liftIO $ GHC.mkSplitUniqSupply 'H'
    let prog = preProcess uniqHoleSupply $ GHC.mg_binds mgCore
    return (prog, parsedSourceCode)


desugarPreprocessSimplification :: ToCore (GHC.CoreProgram, GHC.ParsedSource)
-- |  a haskell file -> desugar pass(including simple optimiser) -> handling type hole error -> core-to-core simplification
desugarPreprocessSimplification = do
  (mgCore, psrc) <- desugarToCore True (holeFlags ++ genFlags ++ simplFlags)
  env <- GHC.getSession
  uniqHoleSupply <- liftIO $ GHC.mkSplitUniqSupply 'H'
  let prog = preProcess uniqHoleSupply (GHC.mg_binds mgCore)
  -- ! Need to examine the utility of core2core
  mgSimpl <- liftIO $ GHC.core2core env (mgCore{GHC.mg_binds = prog})
  return (GHC.mg_binds mgSimpl, psrc)


desugarPreprocessNormalize :: ToCore (GHC.CoreProgram, GHC.ParsedSource, Map.Map GHC.Var GHC.Var)
-- |  a haskell file -> desugar pass(including simple optimiser) -> handling type hole error -> normalization 
desugarPreprocessNormalize = do
    (mgCore, parsedSourceCode) <- desugarToCore False (holeFlags ++ genFlags)
    uniqHoleSupply <- liftIO $ GHC.mkSplitUniqSupply 'H'
    let prog = preProcess uniqHoleSupply $ GHC.mg_binds mgCore
    uniqTopLevelLetRecSupply <- liftIO $ GHC.mkSplitUniqSupply 'R'

    fnName <- liftToCore $ asks getStudentExerciseName
    let (normalizedProg, alphaRenamingMapping) = normalise fnName uniqTopLevelLetRecSupply prog 
    -- liftIO $ putStrLn $ show normalizedProg
    -- liftIO $ putStrLn "\n------------------------------------"
    -- liftIO $ GHC.printSDoc GHC.defaultSDocContext GHC.ZigZagMode stdout (GHC.ppr normalizedProg)
    let removedTypeEvidence = removeTyEvidence normalizedProg
    -- liftIO $ putStrLn "\n--------------Removed Evidence---------------------"
    -- liftIO $ putStrLn $ show removedTypeEvidence
    -- liftIO $ putStrLn "\n------------------------------------"
    -- liftIO $ GHC.printSDoc GHC.defaultSDocContext GHC.ZigZagMode stdout (GHC.ppr removedTypeEvidence)
    return (prog, parsedSourceCode, alphaRenamingMapping)
    
libDirPath :: IO FilePath
libDirPath = init <$> readProcess "ghc" ["--print-libdir"] ""


-- | Error Handling when unintended things happen, like when loading ill-formed haskell file, mainly IO actions.
-- | Hence, we dont have error information like reasons for failing, serverity and source location
-- | SDoc implements IsString typeclass, we can pass string literal to the function
guardS :: Bool -> GHC.SDoc -> ToCoreError -> ToCore ()
guardS True _ _ = pure ()
guardS False sdoc errorType = do
    dynflags <- GHC.getDynFlags
    liftIO $ GHCLogger.defaultLogAction dynflags GHC.NoReason GHC.SevInfo GHC.noSrcSpan sdoc
    liftToCore $ throwError errorType

liftToCore :: ReaderT ToCoreOption (ExceptT ToCoreError IO) a -> ToCore a
liftToCore f = MkToCore $ GHC.liftGhcT f


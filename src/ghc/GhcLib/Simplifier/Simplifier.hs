module GhcLib.Simplifier.Simplifier (test) where

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
import GhcLib.GHC.Driver.Make qualified as GHC
import GHC.Runtime.Interpreter qualified as GHC
import GHC.Conc qualified as GHC


import Control.Monad.Catch
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Exception (evaluate)
import Control.Monad.Reader
import Data.Either
import Data.IORef (IORef, newIORef)
import Data.Maybe
import Data.List
import Data.String (IsString (fromString))

import System.FilePath (takeBaseName)
import System.Process (readProcess)

import GhcLib.Utility.Bag ()
import GhcLib.Utility.Flags
import GhcLib.Utility.Utility
import GhcLib.Utility.Warning



data SimplifyOption = SimplifyOption
    { getSimplifyTargetPath :: FilePath
    , getSimplifyTargetName :: String
    }
    deriving stock (Show)
data SimplifyingError = FailedUnloading | FailedLoading | NotInsideModuleGraph GHC.SDoc | ParsingError GHC.ErrorMessages
    deriving stock (Show)

-- data SimplifyingResult = ParsingError GHC.ErrorMessages | TypeCheckingError | DesugaringError

instance Exception SimplifyingError

-- newtype Simplify a = MkSimplify {runSimplify :: GHC.GhcT (ExceptT SimplifyingError IO) a}
--     deriving newtype (Functor, Applicative, Monad, MonadIO,GHC.HasDynFlags, GHCLogger.HasLogger)
--     deriving (MonadThrow, MonadCatch, MonadMask) via (ReaderT GHC.Session (ExceptT SimplifyingError IO))
--     deriving newtype (GHC.GhcMonad)

-- newtype Wrapper a = MkWrapper {runWrapper :: ReaderT SimplifyOption Simplify a }

newtype Simplify a = MkSimplify {runSimplify :: GHC.GhcT (ReaderT SimplifyOption (ExceptT SimplifyingError IO)) a}
    deriving newtype (Functor, Applicative, Monad, MonadIO, GHC.HasDynFlags, GHCLogger.HasLogger)
    deriving (MonadThrow, MonadCatch, MonadMask) via (ReaderT GHC.Session (ReaderT SimplifyOption (ExceptT SimplifyingError IO)))
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

setFlags :: Bool -> [GHC.GeneralFlag] -> Simplify ()

-- | Set extra dynamic flags(flags from the command line) along with default always-on flags
setFlags b flags = do
    currDynFlags <- GHC.getSessionDynFlags
    let gflags = if b then GHCEnumSet.toList (GHC.generalFlags currDynFlags) ++ flags else flags
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

-- | ghc loads the target file
loadWithoutPlugins :: GHC.Target -> Simplify ()
loadWithoutPlugins targetFile = do
    targetFP <- liftS $ asks getSimplifyTargetPath
    -- first unload all the files (like GHCi :load does)
    GHC.setTargets []
    unloadingFlag <- GHC.load GHC.LoadAllTargets
    guardS (GHC.succeeded unloadingFlag) "Error : Clearing context failed" FailedUnloading
    --  Why do we need to set this option to Nothing?\
    -- traceM $ "Trace point : " ++ (show $ GHC.outputFile_ dflags)
    -- Nothing
    -- GHC.setSessionDynFlags dflags{GHC.outputFile_ = Nothing}
    GHC.setTargets [targetFile]
    -- ! This operation failed if the target is not well-formed haskell file
    -- lo GHC.load GHC.LoadAllTargets
    -- guardS (GHC.succeeded loadingFlag) (fromString $ "Error : Failed to load target : " ++ targetFP) FailedLoading
    hsFile <- liftS $ asks getSimplifyTargetName
    -- ? What does load really do? The description indicates that it parses and typechecks the target file
    -- ? Do we really need to parse and typecheck it again later? or do we need to call `load` , 
    -- ?                  this complex and all-in-one function at this step? Can we do some fine-grained operations?
    -- ?                       some part of examination of load is put inside docs folder
    maybeLoaded <- GHC.handleSourceError (pure . Left . GHC.srcErrorMessages) (Right <$> GHC.load GHC.LoadAllTargets)
    guardS (isRight maybeLoaded) (fromString $ "Error : Failed to parse " ++ hsFile) (ParsingError $ fromLeft GHC.emptyBag maybeLoaded)
    guardS (GHC.succeeded $ fromRight GHC.Succeeded maybeLoaded) (fromString $ "Error : Failed to load target : " ++ targetFP) FailedLoading

-- Main copied from GHC 9.2.8 load function, but keep typecheck result 
-- check more detailed documentations in GHC haddoc page 
customizedLoad :: Simplify ()
customizedLoad  = do 
    (err, mod_graph) <- GHC.depanalE [] False
    
    pure ()

-- >>> take 10 [x | x <- [1..], odd x]


customizedLoad' :: Maybe GHC.Messager -> GHC.ModuleGraph -> Simplify ()
customizedLoad' mHscMessage mod_graph = do
    GHC.modifySession $ \hsc_env -> hsc_env {GHC.hsc_mod_graph = mod_graph}
    -- GHC.guessOutputFile -- ? not sure we need this function
    hsc_env <- GHC.getSession 

    let hpt1 = GHC.hsc_HPT hsc_env 
        dflags = GHC.hsc_dflags hsc_env 
        logger = GHC.hsc_logger hsc_env
        interp = GHC.hscInterp hsc_env

    let all_home_mods =
          -- take all modules which are derived from hs-boot files
          GHC.mkUniqSet [ GHC.ms_mod_name s
                    | s <- GHC.mgModSummaries mod_graph, GHC.isBootSummary s == GHC.NotBoot]

     
    let mg2_with_srcimps :: [GHC.SCC GHC.ModSummary]
        mg2_with_srcimps = GHC.filterToposortToModules $
                    GHC.topSortModuleGraph True mod_graph Nothing

    -- If we can determine that any of the {-# SOURCE #-} imports
    -- are definitely unnecessary, then emit a warning.
    GHC.warnUnnecessarySourceImports mg2_with_srcimps

    let
        -- check the stability property for each module.
        stable_mods@(stable_obj,stable_bco)
            = GHC.checkStability hpt1 mg2_with_srcimps all_home_mods

        pruned_hpt = hpt1

    _ <- liftIO $ evaluate pruned_hpt

    -- before we unload anything, make sure we don't leavedebugTraceMsg  an old
    -- interactive context around pointing to dead bindings.  Also,
    -- write the pruned HPT to allow the old HPT to be GC'd.
    GHC.setSession $ GHC.discardIC $ hsc_env { GHC.hsc_HPT = pruned_hpt }

    let msg = GHC.text "Stable obj:" GHC.<+> GHC.ppr stable_obj GHC.$$
                            GHC.text "Stable BCO:" GHC.<+> GHC.ppr stable_bco
    liftIO $ GHCUtils.debugTraceMsg logger dflags 2 msg

    let stable_linkables = [ linkable
                        | m <- GHC.nonDetEltsUniqSet stable_obj ++
                                GHC.nonDetEltsUniqSet stable_bco,
                            -- It's OK to use nonDetEltsUniqSet here
                            -- because it only affects linking. Besides
                            -- this list only serves as a poor man's set.
                            Just hmi <- [GHC.lookupHpt pruned_hpt m],
                            Just linkable <- [GHC.hm_linkable hmi] ]
    liftIO $ GHC.unload interp hsc_env stable_linkables
    
    let full_mg, partial_mg0, partial_mg, unstable_mg :: [GHC.SCC GHC.ModuleGraphNode]
        stable_mg :: [GHC.SCC GHC.ExtendedModSummary]
        full_mg  = GHC.topSortModuleGraph False mod_graph Nothing

        maybe_top_mod = Nothing
        -- LoadDependenciesOf m: we want the upsweep to stop just
        -- short of the specified module (unless the specified module
        -- is stable).
        partial_mg0 = GHC.topSortModuleGraph False mod_graph maybe_top_mod
        partial_mg = partial_mg0

        stable_mg = [ GHC.AcyclicSCC ems
            | GHC.AcyclicSCC (GHC.ModuleNode ems@(GHC.ExtendedModSummary ms _)) <- full_mg
            , stable_mod_summary ms
            ]
        stable_mod_summary ms =
            GHC.ms_mod_name ms `GHC.elementOfUniqSet` stable_obj ||
            GHC.ms_mod_name ms `GHC.elementOfUniqSet` stable_bco

        -- the modules from partial_mg that are not also stable
        -- NB. also keep cycles, we need to emit an error message later
        unstable_mg = filter not_stable partial_mg
          where not_stable (GHC.CyclicSCC _) = True
                not_stable (GHC.AcyclicSCC (GHC.InstantiationNode _)) = True
                not_stable (GHC.AcyclicSCC (GHC.ModuleNode (GHC.ExtendedModSummary ms _)))
                   = not $ stable_mod_summary ms
        -- Load all the stable modules first, before attempting to load
        -- an unstable module (#7231).
        mg = fmap (fmap GHC.ModuleNode) stable_mg ++ unstable_mg

    liftIO $ GHCUtils.debugTraceMsg logger dflags 2 (GHC.hang (GHC.text "Ready for upsweep")
                               2 (GHC.ppr mg))

    n_jobs <- case GHC.parMakeCount dflags of
                    Nothing -> liftIO GHC.getNumProcessors
                    Just n  -> return n
    -- let upsweep_fn | n_jobs > 1 = parUpsweep n_jobs
    --                | otherwise  = upsweep
        

        

    pure ()
    pure ()



    














initEnv :: Bool -> [GHC.GeneralFlag] -> Simplify (IORef [Warning])
initEnv keepDefaultFlags flags = do
    hsFilePath <- liftS $ asks getSimplifyTargetPath
    setFlags keepDefaultFlags flags
    -- logger <- GHC.getLogger
    -- ! Not sure whether we need this in the future
    ref <- liftIO (newIORef [])
    -- in case of logging, also write it to the IORef
    GHC.pushLogHookM (writeWarnings ref)
    target <- GHC.guessTarget hsFilePath Nothing
    loadWithoutPlugins target
    return ref

-- toDesugar' :: Bool -> [GHC.GeneralFlag] -> Simplify (GHC.ModGuts, GHC.ParsedSource, IORef [Warning])
toDesugar' :: Bool -> [GHC.GeneralFlag] -> Simplify ()

-- | Compile a file to the desugar pass + simple optimiser and return Modguts and warnings
toDesugar' keepExistingFlags flags = do
    hsFile <- liftS $ asks getSimplifyTargetName
    ref <- initEnv keepExistingFlags flags
    modSum <- getMaybeSModSummary $ GHC.mkModuleName hsFile
    -- guardS (isJust modSum) (fromString $ "Error : " ++ hsFile ++ " is not part of the module graph") NotInsideModuleGraph
    -- maybeParsed <-onError (Right <$> GHC.parseModule (fromJust modSum)) (\sourceError -> pure . Left . GHC.srcErrorMessages sourceError)
    -- ! How come handleSourceError fails to catch the exception?
    -- _ <- throwM FailedUnloading `catch` \e -> liftIO $ putStrLn ("Caught " ++ show (e :: SimplifyingError))
    -- maybeParsed <- GHC.handleSourceError (pure . Left . GHC.srcErrorMessages) (Right <$> GHC.parseModule (fromJust modSum))
    -- guardS (isRight maybeParsed) (fromString $ "Error : Failed to parse " ++ hsFile) (ParsingError $ fromLeft GHC.emptyBag maybeParsed)

    -- tmod <- GHC.typecheckModule pmod
    -- -- let tprog = attachNote (tm_typechecked_source tmod)
    -- dmod <- GHC.desugarModule tmod -- (tmod {tm_typechecked_source = tprog})
    -- let modguts = GHC.coreModule dmod
    -- -- cprog <- liftIO $ preProcess (mg_binds modguts) -- apply preprocessing transformations
    -- -- let mg = modguts {mg_binds = cprog}
    -- return (modguts, GHC.pm_parsed_source pmod, ref)
    pure ()

libDirPath :: IO FilePath
libDirPath = init <$> readProcess "ghc" ["--print-libdir"] ""

-- test' :: ExceptT SimplifyingError IO ()
test' :: ReaderT SimplifyOption (ExceptT SimplifyingError IO) ()
test' = do
    libDirPath' <- liftIO libDirPath
    logging <- GHC.defaultErrorHandler
        GHC.defaultFatalMessager
        GHC.defaultFlushOut
        $ GHC.runGhcT (Just libDirPath')
        $ runSimplify
        $ do
            _ <- toDesugar' True []
            let test = 0
            pure ()
    pure ()

test :: IO ()
test = do
    let target = "./ghcTestCases/Test.hs"
    result <- runExceptT (runReaderT test' $ SimplifyOption target (takeBaseName target))
    case result of
        Left e -> print e
        Right _ -> pure ()

-- | Error Handling when unintended things happen, like when loading ill-formed haskell file, mainly IO actions.
-- | Hence, we dont have error information like reasons for failing, serverity and source location
-- | SDoc implements IsString typeclass, we can pass string literal to the function
guardS :: Bool -> GHC.SDoc -> SimplifyingError -> Simplify ()
guardS True _ _ = pure ()
guardS False sdoc errorType = do
    dynflags <- GHC.getDynFlags
    liftIO $ GHCLogger.defaultLogAction dynflags GHC.NoReason GHC.SevInfo GHC.noSrcSpan sdoc
    liftS $ throwError errorType

liftS :: ReaderT SimplifyOption (ExceptT SimplifyingError IO) a -> Simplify a
liftS f = MkSimplify $ GHC.liftGhcT f


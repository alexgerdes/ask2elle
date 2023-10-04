

module GhcLib.Simplifier.Simplifier (test) where

import Control.Monad.Catch
import Data.IORef (IORef, newIORef)
import Debug.Trace
import GHC qualified
import GHC.Data.EnumSet qualified as GHCEnumSet
import GHC.Driver.Monad qualified as GHC
import GHC.Driver.Session qualified as GHC
import GHC.IO (unsafePerformIO)
import GHC.LanguageExtensions.Type qualified as GHC
import GHC.Plugins qualified as GHC
import GHC.Utils.Logger qualified as GHCLogger
import System.FilePath (takeBaseName)
import System.Process (readProcess)

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Reader

import GhcLib.Utility.Flags
import GhcLib.Utility.Warning

data SimplifyingError = FailedUnloading | FailedLoading
    deriving stock (Show)

instance Exception SimplifyingError

newtype Simplify a = MkSimplify {runSimplify :: GHC.GhcT (ExceptT SimplifyingError IO) a}
    deriving newtype (Functor, Applicative, Monad, MonadIO, GHC.HasDynFlags, GHCLogger.HasLogger)
    deriving (MonadThrow, MonadCatch, MonadMask) via (ReaderT GHC.Session (ExceptT SimplifyingError IO))
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

loadWithPlugins :: GHC.Target -> [GHC.StaticPlugin] -> Simplify GHC.SuccessFlag

-- |  ghc loads the target file with a list of plugins
loadWithPlugins targetFile plugins = do
    -- first unload all the files (like GHCi :load does)
    GHC.setTargets []
    unloadingFlag <- GHC.load GHC.LoadAllTargets

    -- guardM (GHC.succeeded unloadingFlag) FailedUnloading
    -- actually start loading file
    GHC.setTargets [targetFile]
    dflags <- GHC.getSessionDynFlags
    GHC.modifySession $ \hsc_env ->
        hsc_env{GHC.hsc_static_plugins = GHC.hsc_static_plugins hsc_env ++ plugins} -- set new plugins
    GHC.setSessionDynFlags dflags{GHC.outputFile_ = Nothing}
    GHC.load GHC.LoadAllTargets

-- | ghc loads the target file
loadWithoutPlugins :: GHC.Target -> Simplify ()
loadWithoutPlugins targetFile = do
    -- first unload all the files (like GHCi :load does)
    GHC.setTargets []
    unloadingFlag <- GHC.load GHC.LoadAllTargets
    when
        (GHC.failed unloadingFlag)
        ( do
            dynflags <- GHC.getDynFlags
            liftIO $ GHCLogger.defaultLogAction dynflags GHC.NoReason GHC.SevInfo GHC.noSrcSpan "Error : Clearing context failed"
            MkSimplify $ GHC.liftGhcT $ throwError FailedUnloading
        )
    --  Why do we need to set this option to Nothing?\
    -- traceM $ "Trace point : " ++ (show $ GHC.outputFile_ dflags)
    -- Nothing
    -- GHC.setSessionDynFlags dflags{GHC.outputFile_ = Nothing}
    GHC.setTargets [targetFile]
    -- ! This operation failed if the target is not well-formed haskell file
    let moduleName = GHC.mkModuleName (takeBaseName "./heliumTestCases/Success/correct/Abs.hs")
    loadingFlag <- GHC.load $ GHC.LoadAllTargets
    modSum <- GHC.getModSummary moduleName

    traceM $ show $ GHC.ms_location modSum
    traceM $ show $ GHC.ms_hsc_src modSum
    traceM $ show $ GHC.succeeded loadingFlag

    when
        (GHC.failed loadingFlag)
        ( do
            dynflags <- GHC.getDynFlags
            liftIO $ GHCLogger.defaultLogAction dynflags GHC.NoReason GHC.SevInfo GHC.noSrcSpan "Error : Loading failed"
            MkSimplify $ GHC.liftGhcT $ throwError FailedLoading
        )

initEnv :: Bool -> [GHC.GeneralFlag] -> FilePath -> Simplify (IORef [Warning])
initEnv keepDefaultFlags flags hsFile = do
    setFlags keepDefaultFlags flags
    -- logger <- GHC.getLogger
    ref <- liftIO (newIORef [])
    -- in case of logging, also write it to the IORef
    GHC.pushLogHookM (writeWarnings ref)

    target <- GHC.guessTarget hsFile Nothing
    loadWithoutPlugins target
    return ref

toDesugar' :: Bool -> [GHC.GeneralFlag] -> FilePath -> Simplify (GHC.ModGuts, GHC.ParsedSource, IORef [Warning])

-- | Compile a file to the desugar pass + simple optimiser and return Modguts and warnings
toDesugar' setdefaultFlags flags targetFilePath = do
    -- ! start at this next time, reason what each instruction does, why loads all targets twice, just for the flag?
    ref <- initEnv setdefaultFlags flags targetFilePath
    modSum <- GHC.getModSummary $ GHC.mkModuleName (takeBaseName targetFilePath)
    pmod <- GHC.parseModule modSum
    tmod <- GHC.typecheckModule pmod
    -- let tprog = attachNote (tm_typechecked_source tmod)
    dmod <- GHC.desugarModule tmod -- (tmod {tm_typechecked_source = tprog})
    let modguts = GHC.coreModule dmod
    -- cprog <- liftIO $ preProcess (mg_binds modguts) -- apply preprocessing transformations
    -- let mg = modguts {mg_binds = cprog}
    return (modguts, GHC.pm_parsed_source pmod, ref)

libDirPath :: FilePath
libDirPath = init $ unsafePerformIO (readProcess "ghc" ["--print-libdir"] "")

test' :: ExceptT SimplifyingError IO ()
test' = do
    let target = "./heliumTestCases/Success/correct/Abs.hs"
    logging <- GHC.defaultErrorHandler
        GHC.defaultFatalMessager
        GHC.defaultFlushOut
        $ GHC.runGhcT (Just libDirPath)
        $ runSimplify
        $ do
            ref <- initEnv True [] target
            modSum <- GHC.getModSummary $ GHC.mkModuleName (takeBaseName target)
            traceM $ show $ GHC.ms_hs_date modSum
            pure ref
    pure ()

test :: IO ()
test = do
    result <- runExceptT test'
    case result of
        Left e -> print e
        Right _ -> pure ()
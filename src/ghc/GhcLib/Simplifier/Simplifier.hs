{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module GhcLib.Simplifier.Simplifier (test) where

import Control.Monad.Catch
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Foldable (foldl')
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

import GHC.Exception (Exception)
import GhcLib.Utility.Flags
import GhcLib.Utility.Warning
import Control.Monad.Except (ExceptT)
import Control.Monad (unless)
import Control.Monad (when)

data SimplifyingError = FailedUnloading | FailedLoading
    deriving stock (Show)

instance Exception SimplifyingError

instance Show GHC.SuccessFlag where
    show :: GHC.SuccessFlag -> String
    show GHC.Succeeded = "Succeeded"
    show GHC.Failed = "Failed"



newtype Simplify a = MkSimplify {runSimplify :: GHC.GhcT (Either SimplifyingError)  a}
    deriving newtype (Functor, Applicative, Monad)


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

setFlags :: Bool -> [GHC.GeneralFlag] -> GHC.Ghc ()

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

loadWithPlugins :: GHC.GhcMonad m => GHC.Target -> [GHC.StaticPlugin] -> m GHC.SuccessFlag

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
loadWithoutPlugins :: GHC.GhcMonad m => GHC.Target -> m ()
loadWithoutPlugins targetFile = do
    -- first unload all the files (like GHCi :load does)
    GHC.setTargets []
    unloadingFlag <- GHC.load GHC.LoadAllTargets
    traceM $ show unloadingFlag
    when (GHC.succeeded unloadingFlag) (do
        dynflags <- GHC.getDynFlags
        liftIO $ GHCLogger.defaultLogAction dynflags GHC.NoReason GHC.SevInfo GHC.noSrcSpan "Hello World"
        throwM FailedLoading)

    -- guardM (GHC.succeeded unloadingFlag) FailedUnloading

    -- dflags <- GHC.getSessionDynFlags
    --  Why do we need to set this option to Nothing?\
    -- traceM $ "Trace point : " ++ (show $ GHC.outputFile_ dflags)
    -- Nothing
    -- GHC.setSessionDynFlags dflags{GHC.outputFile_ = Nothing}
    GHC.setTargets [targetFile]
    -- ! Why the following load always fails?export LD_LIBRARY_PATH=${pkgs.gcc.cc.lib}/lib:$LD_LIBRARY_PATH
    loadingFlag <- GHC.load GHC.LoadAllTargets
    traceM $ show loadingFlag
    -- guardM (GHC.succeeded loadingFlag) FailedLoading

    pure ()


initEnv :: Bool -> [GHC.GeneralFlag] -> FilePath -> GHC.Ghc (IORef [Warning])
initEnv keepDefaultFlags flags hsFile = do
    setFlags keepDefaultFlags flags
    -- logger <- GHC.getLogger
    ref <- liftIO (newIORef [])
    -- in case of logging, also write it to the IORef
    GHC.pushLogHookM (writeWarnings ref)

    target <- GHC.guessTarget hsFile Nothing
    loadWithoutPlugins target
    return ref

toDesugar' :: Bool -> [GHC.GeneralFlag] -> FilePath -> GHC.Ghc (GHC.ModGuts, GHC.ParsedSource, IORef [Warning])

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

test :: IO ()
test = do
    let target = "./heliumTestCases/Success/correct/Abs.hs"
    logging <- GHC.defaultErrorHandler
        GHC.defaultFatalMessager
        GHC.defaultFlushOut
        $ GHC.runGhc (Just libDirPath)
        $ do
            ref <- initEnv True [] target
            modSum <- GHC.getModSummary $ GHC.mkModuleName (takeBaseName target)
            traceM $ show $ GHC.ms_hs_date modSum
            pure ref
    pure ()


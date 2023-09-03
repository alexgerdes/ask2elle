module GhcLib.Simplifier.Simplifier (test) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.IORef (IORef, newIORef)
import GHC qualified
import GHC.Data.EnumSet qualified as GHCEnumSet
import GHC.Driver.Monad qualified as GHC
import GHC.Driver.Session qualified as GHC
import GHC.LanguageExtensions.Type qualified as GHC
import GHC.Plugins qualified as GHC
import System.FilePath (takeBaseName)

import Data.Foldable (foldl')
import GhcLib.Utility.Flags
import GhcLib.Utility.Warning

setExtensionFlag' :: GHC.Extension -> GHC.DynFlags -> GHC.DynFlags

-- | Reimplementation of setExtensionFlag' as ghc doesn't export it
setExtensionFlag' f dflags = foldr ($) (GHC.xopt_set dflags f) deps
  where
    deps =
        [ if turn_on
            then setExtensionFlag' d
            else unSetExtensionFlag' d
        | (f', turn_on, d) <- GHC.impliedXFlags
        , f' == f
        ]

-- When you set f, set the ones it implies
-- NB: use setExtensionFlag recursively, in case the implied flags
--     implies further flags

unSetExtensionFlag' :: GHC.Extension -> GHC.DynFlags -> GHC.DynFlags

-- | Reimplementation of unSetExtensionFlag' as ghc doesn't export it
unSetExtensionFlag' f dflags = GHC.xopt_unset dflags f

-- When you un-set f, however, we don't un-set the things it implies
--      (except for -fno-glasgow-exts, which is treated specially)

setFlags :: Bool -> [GHC.GeneralFlag] -> GHC.Ghc ()

-- setGeneralFlag' :: GeneralFlag -> DynFlags -> DynFlags

-- setExtensionFlag', unSetExtensionFlag' :: LangExt.Extension -> DynFlags -> DynFlags

-- | Set dynamic flags
setFlags b flags = do
    currDynFlags <- GHC.getSessionDynFlags
    let gflags = if b then GHCEnumSet.toList (GHC.generalFlags currDynFlags) ++ flags else flags
        targetDynFlagStatus =
            ( flip (foldl (flip GHC.unSetGeneralFlag')) unsetGenFlags
                -- unset general flags
                . flip (foldl GHC.wopt_unset) unsetWarnFlags
                -- suppress warning flags
                . flip (foldl GHC.wopt_set) setWarnFlags
                -- set warning flags
                . flip (foldl (flip setExtensionFlag')) extFlags
                -- enable ghc extensions
                . flip (foldl (flip unSetExtensionFlag')) unsetExtFlags
                -- disable ghc extensions
                . flip (foldl (flip GHC.setGeneralFlag')) gflags
                -- set general flags
            )
                currDynFlags

    {- to update DynFlags directly
    dflags' = dflags {refLevelHoleFits = Just 2,
                      maxValidHoleFits = Just 8,
                      maxRefHoleFits   = Just 10}
    -}

    GHC.setSessionDynFlags targetDynFlagStatus

loadWithPlugins :: GHC.GhcMonad m => GHC.DynFlags -> GHC.Target -> [GHC.StaticPlugin] -> m GHC.SuccessFlag

-- |  ghc loads the target file with a list of plugins
-- ! start at this next time, reason what each instruction does, why loads all targets twice, just for the flag?
loadWithPlugins dflags t plugins = do
    -- first unload (like GHCi :load does)
    GHC.setTargets []
    _ <- GHC.load GHC.LoadAllTargets
    GHC.setTargets [t]
    GHC.modifySession $ \hsc_env ->
        let old_plugins = GHC.hsc_static_plugins hsc_env
        in  hsc_env{GHC.hsc_static_plugins = old_plugins ++ plugins} -- set new plugins
    GHC.setSessionDynFlags dflags{GHC.outputFile_ = Nothing}
    GHC.load GHC.LoadAllTargets

-- | ghc loads the target file
loadWithoutPlugins :: GHC.GhcMonad m => GHC.DynFlags -> GHC.Target -> m GHC.SuccessFlag

-- |  ghc loads the target file with a list of plugins
loadWithoutPlugins dflags t = do
    -- first unload (like GHCi :load does)
    GHC.setTargets []
    _ <- GHC.load GHC.LoadAllTargets
    GHC.setTargets [t]
    GHC.setSessionDynFlags dflags{GHC.outputFile_ = Nothing}
    GHC.load GHC.LoadAllTargets

initEnv :: Bool -> [GHC.GeneralFlag] -> FilePath -> GHC.Ghc (IORef [Warning])
initEnv keepDefaultFlags flags fp = do
    -- env <- getSession
    setFlags keepDefaultFlags flags
    ref <- liftIO (newIORef [])
    GHC.pushLogHookM (writeWarnings ref)

    -- target <- GHC.guessTarget fp Nothing
    -- dflags <- GHC.getSessionDynFlags
    -- GHC.loadWithPlugins
    --     dflags
    --     target
    --     [ GHC.StaticPlugin $
    --         GHC.PluginWithArgs
    --             { GHC.paArguments = []
    --             , GHC.paPlugin = plugin -- hlint plugin
    --             }
    --     ]
    return ref

toDesugar' :: Bool -> [GHC.GeneralFlag] -> FilePath -> GHC.Ghc (GHC.ModGuts, GHC.ParsedSource, IORef [Warning])

-- | Compile a file to the desugar pass + simple optimiser and return Modguts and warnings
toDesugar' setdefaultFlags flags fp = do
    ref <- initEnv setdefaultFlags flags fp
    modSum <- GHC.getModSummary $ GHC.mkModuleName (takeBaseName fp)
    pmod <- GHC.parseModule modSum
    tmod <- GHC.typecheckModule pmod
    -- let tprog = attachNote (tm_typechecked_source tmod)
    dmod <- GHC.desugarModule tmod -- (tmod {tm_typechecked_source = tprog})
    let modguts = GHC.coreModule dmod
    -- cprog <- liftIO $ preProcess (mg_binds modguts) -- apply preprocessing transformations
    -- let mg = modguts {mg_binds = cprog}
    return (modguts, GHC.pm_parsed_source pmod, ref)

test :: Int
test = 1
-- ! Right now this module is not needed 

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- -----------------------------------------------------------------------------
--
-- (c) The University of Glasgow, 2011
--
-- This module implements multi-module compilation, and is used
-- by --make and GHCi.
--
-- -----------------------------------------------------------------------------
module GhcLib.GHC.Driver.Make (
        -- depanal, depanalE, depanalPartial,
        -- load, load', LoadHowMuch(..),
        instantiationNodes,

        -- downsweep,

        topSortModuleGraph,

        ms_home_srcimps, ms_home_imps,

        -- summariseModule,
        hscSourceToIsBoot,
        findExtraSigImports,
        implicitRequirementsShallow,

        noModError, cyclicModuleErr,
        moduleGraphNodes, SummaryNode,
        IsBootInterface(..),

        ModNodeMap(..), emptyModNodeMap, modNodeMapElems, modNodeMapLookup, modNodeMapInsert
    ) where

#include "HsVersions.h"

import GHC.Prelude

import GHC.Tc.Utils.Backpack
import GHC.Tc.Utils.Monad  ( initIfaceCheck )

import GHC.Runtime.Interpreter
import qualified GHC.Linker.Loader as Linker
import GHC.Linker.Types

import GHC.Runtime.Context

import GHC.Driver.Config
import GHC.Driver.Phases
import GHC.Driver.Pipeline
import GHC.Driver.Session
import GHC.Driver.Backend
import GHC.Driver.Monad
import GHC.Driver.Env
import GHC.Driver.Errors
import GHC.Driver.Main

import GHC.Parser.Header
import GHC.Parser.Errors.Ppr

import GHC.Iface.Load      ( cannotFindModule )
import GHC.IfaceToCore     ( typecheckIface )
import GHC.Iface.Recomp    ( RecompileRequired ( MustCompile ) )

import GHC.Data.Bag        ( unitBag, listToBag, unionManyBags, isEmptyBag )
import GHC.Data.Graph.Directed
import GHC.Data.FastString
import GHC.Data.Maybe      ( expectJust )
import GHC.Data.StringBuffer
--import qualified GHC.LanguageExtensions as LangExt

import GHC.Utils.Exception ( tryIO )
import GHC.Utils.Monad     ( allM )
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Misc
import GHC.Utils.Error
import GHC.Utils.Logger
import GHC.Utils.TmpFs

import GHC.Types.Basic
import GHC.Types.Target
import GHC.Types.SourceFile
import GHC.Types.SourceError
import GHC.Types.SrcLoc
import GHC.Types.Unique.DSet
import GHC.Types.Unique.Set
import GHC.Types.Name
import GHC.Types.Name.Env

import GHC.Unit
import GHC.Unit.State
import GHC.Unit.Finder
import GHC.Unit.Module.ModSummary
import GHC.Unit.Module.ModIface
import GHC.Unit.Module.Graph
import GHC.Unit.Home.ModInfo

import Data.Either ( rights, partitionEithers )
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import qualified GHC.Data.FiniteMap as Map ( insertListWith )

import Control.Concurrent ( forkIOWithUnmask, killThread )
import qualified GHC.Conc as CC
import Control.Concurrent.MVar
import Control.Concurrent.QSem
import Control.Exception
import Control.Monad
-- import Control.Monad.Trans.Except ( ExceptT(..), runExceptT, throwE )
import qualified Control.Monad.Catch as MC
import Data.IORef
import Data.List (nub, sortBy, partition)
import qualified Data.List as List
import Data.Foldable (toList)
import Data.Maybe
import Data.Ord ( comparing )
-- import Data.Time
import Data.Bifunctor (first)
-- import System.Directory
import System.FilePath
import System.IO        ( fixIO )
import System.IO.Error  ( isDoesNotExistError )

import GHC.Conc ( getNumProcessors, getNumCapabilities, setNumCapabilities )

label_self :: String -> IO ()
label_self thread_name = do
    self_tid <- CC.myThreadId
    CC.labelThread self_tid thread_name

-- -----------------------------------------------------------------------------
-- Loading the program

-- | Perform a dependency analysis starting from the current targets
-- and update the session with the new module graph.
--
-- Dependency analysis entails parsing the @import@ directives and may
-- therefore require running certain preprocessors.
--
-- Note that each 'ModSummary' in the module graph caches its 'DynFlags'.
-- These 'DynFlags' are determined by the /current/ session 'DynFlags' and the
-- @OPTIONS@ and @LANGUAGE@ pragmas of the parsed module.  Thus if you want
-- changes to the 'DynFlags' to take effect you need to call this function
-- again.
-- In case of errors, just throw them.
--
-- depanal :: GhcMonad m =>
--            [ModuleName]  -- ^ excluded modules
--         -> Bool          -- ^ allow duplicate roots
--         -> m ModuleGraph
-- depanal excluded_mods allow_dup_roots = do
--     (errs, mod_graph) <- depanalE excluded_mods allow_dup_roots
--     if isEmptyBag errs
--       then pure mod_graph
--       else throwErrors errs

-- -- | Perform dependency analysis like in 'depanal'.
-- -- In case of errors, the errors and an empty module graph are returned.
-- depanalE :: GhcMonad m =>     -- New for #17459
--             [ModuleName]      -- ^ excluded modules
--             -> Bool           -- ^ allow duplicate roots
--             -> m (ErrorMessages, ModuleGraph)
-- depanalE excluded_mods allow_dup_roots = do
--     hsc_env <- getSession
--     (errs, mod_graph) <- depanalPartial excluded_mods allow_dup_roots
--     if isEmptyBag errs
--       then do
--         let unused_home_mod_err = warnMissingHomeModules hsc_env mod_graph
--             unused_pkg_err = warnUnusedPackages hsc_env mod_graph
--             warns = unused_home_mod_err ++ unused_pkg_err
--         when (not $ null warns) $
--           logWarnings (listToBag warns)
--         setSession hsc_env { hsc_mod_graph = mod_graph }
--         pure (errs, mod_graph)
--       else do
--         -- We don't have a complete module dependency graph,
--         -- The graph may be disconnected and is unusable.
--         setSession hsc_env { hsc_mod_graph = emptyMG }
--         pure (errs, emptyMG)


-- | Perform dependency analysis like 'depanal' but return a partial module
-- graph even in the face of problems with some modules.
--
-- Modules which have parse errors in the module header, failing
-- preprocessors or other issues preventing them from being summarised will
-- simply be absent from the returned module graph.
--
-- Unlike 'depanal' this function will not update 'hsc_mod_graph' with the
-- new module graph.
-- depanalPartial
--     :: GhcMonad m
--     => [ModuleName]  -- ^ excluded modules
--     -> Bool          -- ^ allow duplicate roots
--     -> m (ErrorMessages, ModuleGraph)
--     -- ^ possibly empty 'Bag' of errors and a module graph.
-- depanalPartial excluded_mods allow_dup_roots = do
--   hsc_env <- getSession
--   let
--          dflags  = hsc_dflags hsc_env
--          targets = hsc_targets hsc_env
--          old_graph = hsc_mod_graph hsc_env
--          logger  = hsc_logger hsc_env

--   withTiming logger dflags (text "Chasing dependencies") (const ()) $ do
--     liftIO $ debugTraceMsg logger dflags 2 (hcat [
--               text "Chasing modules from: ",
--               hcat (punctuate comma (map pprTarget targets))])

--     -- Home package modules may have been moved or deleted, and new
--     -- source files may have appeared in the home package that shadow
--     -- external package modules, so we have to discard the existing
--     -- cached finder data.
--     liftIO $ flushFinderCaches hsc_env

--     mod_summariesE <- liftIO $ downsweep
--       hsc_env (mgExtendedModSummaries old_graph)
--       excluded_mods allow_dup_roots
--     let
--       (errs, mod_summaries) = partitionEithers mod_summariesE
--       mod_graph = mkModuleGraph' $
--         fmap ModuleNode mod_summaries ++ instantiationNodes (hsc_units hsc_env)
--     return (unionManyBags errs, mod_graph)

-- | Collect the instantiations of dependencies to create 'InstantiationNode' work graph nodes.
-- These are used to represent the type checking that is done after
-- all the free holes (sigs in current package) relevant to that instantiation
-- are compiled. This is necessary to catch some instantiation errors.
--
-- In the future, perhaps more of the work of instantiation could be moved here,
-- instead of shoved in with the module compilation nodes. That could simplify
-- backpack, and maybe hs-boot too.
instantiationNodes :: UnitState -> [ModuleGraphNode]
instantiationNodes unit_state = InstantiationNode <$> iuids_to_check
  where
    iuids_to_check :: [InstantiatedUnit]
    iuids_to_check =
      nubSort $ concatMap goUnitId (explicitUnits unit_state)
     where
      goUnitId uid =
        [ recur
        | VirtUnit indef <- [uid]
        , inst <- instUnitInsts indef
        , recur <- (indef :) $ goUnitId $ moduleUnit $ snd inst
        ]

-- Note [Missing home modules]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Sometimes user doesn't want GHC to pick up modules, not explicitly listed
-- in a command line. For example, cabal may want to enable this warning
-- when building a library, so that GHC warns user about modules, not listed
-- neither in `exposed-modules`, nor in `other-modules`.
--
-- Here "home module" means a module, that doesn't come from an other package.
--
-- For example, if GHC is invoked with modules "A" and "B" as targets,
-- but "A" imports some other module "C", then GHC will issue a warning
-- about module "C" not being listed in a command line.
--
-- The warning in enabled by `-Wmissing-home-modules`. See #13129
warnMissingHomeModules :: HscEnv -> ModuleGraph -> [MsgEnvelope DecoratedSDoc]
warnMissingHomeModules hsc_env mod_graph =
    if (wopt Opt_WarnMissingHomeModules dflags && not (null missing))
    then [warn]
    else []
  where
    dflags = hsc_dflags hsc_env
    targets = map targetId (hsc_targets hsc_env)

    is_known_module mod = any (is_my_target mod) targets

    -- We need to be careful to handle the case where (possibly
    -- path-qualified) filenames (aka 'TargetFile') rather than module
    -- names are being passed on the GHC command-line.
    --
    -- For instance, `ghc --make src-exe/Main.hs` and
    -- `ghc --make -isrc-exe Main` are supposed to be equivalent.
    -- Note also that we can't always infer the associated module name
    -- directly from the filename argument.  See #13727.
    is_my_target mod (TargetModule name)
      = moduleName (ms_mod mod) == name
    is_my_target mod (TargetFile target_file _)
      | Just mod_file <- ml_hs_file (ms_location mod)
      = target_file == mod_file ||

           --  Don't warn on B.hs-boot if B.hs is specified (#16551)
           addBootSuffix target_file == mod_file ||

           --  We can get a file target even if a module name was
           --  originally specified in a command line because it can
           --  be converted in guessTarget (by appending .hs/.lhs).
           --  So let's convert it back and compare with module name
           mkModuleName (fst $ splitExtension target_file)
            == moduleName (ms_mod mod)
    is_my_target _ _ = False

    missing = map (moduleName . ms_mod) $
      filter (not . is_known_module) (mgModSummaries mod_graph)

    msg
      | gopt Opt_BuildingCabalPackage dflags
      = hang
          (text "These modules are needed for compilation but not listed in your .cabal file's other-modules: ")
          4
          (sep (map ppr missing))
      | otherwise
      =
        hang
          (text "Modules are not listed in command line but needed for compilation: ")
          4
          (sep (map ppr missing))
    warn = makeIntoWarning
      (Reason Opt_WarnMissingHomeModules)
      (mkPlainMsgEnvelope noSrcSpan msg)

-- | Describes which modules of the module graph need to be loaded.
data LoadHowMuch
   = LoadAllTargets
     -- ^ Load all targets and its dependencies.
   | LoadUpTo ModuleName
     -- ^ Load only the given module and its dependencies.
   | LoadDependenciesOf ModuleName
     -- ^ Load only the dependencies of the given module, but not the module
     -- itself.

-- | Try to load the program.  See 'LoadHowMuch' for the different modes.
--
-- This function implements the core of GHC's @--make@ mode.  It preprocesses,
-- compiles and loads the specified modules, avoiding re-compilation wherever
-- possible.  Depending on the backend (see 'DynFlags.backend' field) compiling
-- and loading may result in files being created on disk.
--
-- Calls the 'defaultWarnErrLogger' after each compiling each module, whether
-- successful or not.
--
-- If errors are encountered during dependency analysis, the module `depanalE`
-- returns together with the errors an empty ModuleGraph.
-- After processing this empty ModuleGraph, the errors of depanalE are thrown.
-- All other errors are reported using the 'defaultWarnErrLogger'.
--
-- load :: GhcMonad m => LoadHowMuch -> m SuccessFlag
-- load how_much = do
--     (errs, mod_graph) <- depanalE [] False                        -- #17459
--     success <- load' how_much (Just batchMsg) mod_graph
--     if isEmptyBag errs
--       then pure success
--       else throwErrors errs

-- Note [Unused packages]
--
-- Cabal passes `--package-id` flag for each direct dependency. But GHC
-- loads them lazily, so when compilation is done, we have a list of all
-- actually loaded packages. All the packages, specified on command line,
-- but never loaded, are probably unused dependencies.

warnUnusedPackages :: HscEnv -> ModuleGraph -> [MsgEnvelope DecoratedSDoc]
warnUnusedPackages hsc_env mod_graph =
    let dflags = hsc_dflags hsc_env
        state  = hsc_units hsc_env

    -- Only need non-source imports here because SOURCE imports are always HPT
        loadedPackages = concat $
          mapMaybe (\(fs, mn) -> lookupModulePackage state (unLoc mn) fs)
            $ concatMap ms_imps (mgModSummaries mod_graph)

        requestedArgs = mapMaybe packageArg (packageFlags dflags)

        unusedArgs
          = filter (\arg -> not $ any (matching state arg) loadedPackages)
                   requestedArgs

        warn = makeIntoWarning
          (Reason Opt_WarnUnusedPackages)
          (mkPlainMsgEnvelope noSrcSpan msg)
        msg = vcat [ text "The following packages were specified" <+>
                     text "via -package or -package-id flags,"
                   , text "but were not needed for compilation:"
                   , nest 2 (vcat (map (withDash . pprUnusedArg) unusedArgs)) ]

    in if not (null unusedArgs) && wopt Opt_WarnUnusedPackages dflags
       then [warn]
       else []

    where
        packageArg (ExposePackage _ arg _) = Just arg
        packageArg _ = Nothing

        pprUnusedArg (PackageArg str) = text str
        pprUnusedArg (UnitIdArg uid) = ppr uid

        withDash = (<+>) (text "-")

        matchingStr :: String -> UnitInfo -> Bool
        matchingStr str p
                =  str == unitPackageIdString p
                || str == unitPackageNameString p

        matching :: UnitState -> PackageArg -> UnitInfo -> Bool
        matching _ (PackageArg str) p = matchingStr str p
        matching state (UnitIdArg uid) p = uid == realUnit state p

        -- For wired-in packages, we have to unwire their id,
        -- otherwise they won't match package flags
        realUnit :: UnitState -> UnitInfo -> Unit
        realUnit state
          = unwireUnit state
          . RealUnit
          . Definite
          . unitId

-- | Generalized version of 'load' which also supports a custom
-- 'Messager' (for reporting progress) and 'ModuleGraph' (generally
-- produced by calling 'depanal'.
load' :: GhcMonad m => LoadHowMuch -> Maybe Messager -> ModuleGraph -> m SuccessFlag
load' how_much mHscMessage mod_graph = do
    modifySession $ \hsc_env -> hsc_env { hsc_mod_graph = mod_graph }
    guessOutputFile
    hsc_env <- getSession

    let hpt1   = hsc_HPT hsc_env
    let dflags = hsc_dflags hsc_env
    let logger = hsc_logger hsc_env
    let interp = hscInterp hsc_env

    -- The "bad" boot modules are the ones for which we have
    -- B.hs-boot in the module graph, but no B.hs
    -- The downsweep should have ensured this does not happen
    -- (see msDeps)
    let all_home_mods =
          mkUniqSet [ ms_mod_name s
                    | s <- mgModSummaries mod_graph, isBootSummary s == NotBoot]
    -- TODO: Figure out what the correct form of this assert is. It's violated
    -- when you have HsBootMerge nodes in the graph: then you'll have hs-boot
    -- files without corresponding hs files.
    --  bad_boot_mods = [s        | s <- mod_graph, isBootSummary s,
    --                              not (ms_mod_name s `elem` all_home_mods)]
    -- ASSERT( null bad_boot_mods ) return ()

    -- check that the module given in HowMuch actually exists, otherwise
    -- topSortModuleGraph will bomb later.
    let checkHowMuch (LoadUpTo m)           = checkMod m
        checkHowMuch (LoadDependenciesOf m) = checkMod m
        checkHowMuch _ = id

        checkMod m and_then
            | m `elementOfUniqSet` all_home_mods = and_then
            | otherwise = do
                    liftIO $ errorMsg logger dflags
                        (text "no such module:" <+> quotes (ppr m))
                    return Failed

    checkHowMuch how_much $ do

    -- mg2_with_srcimps drops the hi-boot nodes, returning a
    -- graph with cycles.  Among other things, it is used for
    -- backing out partially complete cycles following a failed
    -- upsweep, and for removing from hpt all the modules
    -- not in strict downwards closure, during calls to compile.
    let mg2_with_srcimps :: [SCC ModSummary]
        mg2_with_srcimps = filterToposortToModules $
          topSortModuleGraph True mod_graph Nothing

    -- If we can determine that any of the {-# SOURCE #-} imports
    -- are definitely unnecessary, then emit a warning.
    warnUnnecessarySourceImports mg2_with_srcimps

    let
        -- check the stability property for each module.
        stable_mods@(stable_obj,stable_bco)
            = checkStability hpt1 mg2_with_srcimps all_home_mods

        pruned_hpt = hpt1

    _ <- liftIO $ evaluate pruned_hpt

    -- before we unload anything, make sure we don't leave an old
    -- interactive context around pointing to dead bindings.  Also,
    -- write the pruned HPT to allow the old HPT to be GC'd.
    setSession $ discardIC $ hsc_env { hsc_HPT = pruned_hpt }

    liftIO $ debugTraceMsg logger dflags 2 (text "Stable obj:" <+> ppr stable_obj $$
                            text "Stable BCO:" <+> ppr stable_bco)

    -- Unload any modules which are going to be re-linked this time around.
    let stable_linkables = [ linkable
                           | m <- nonDetEltsUniqSet stable_obj ++
                                  nonDetEltsUniqSet stable_bco,
                             -- It's OK to use nonDetEltsUniqSet here
                             -- because it only affects linking. Besides
                             -- this list only serves as a poor man's set.
                             Just hmi <- [lookupHpt pruned_hpt m],
                             Just linkable <- [hm_linkable hmi] ]
    liftIO $ unload interp hsc_env stable_linkables

    -- We could at this point detect cycles which aren't broken by
    -- a source-import, and complain immediately, but it seems better
    -- to let upsweep_mods do this, so at least some useful work gets
    -- done before the upsweep is abandoned.
    --hPutStrLn stderr "after tsort:\n"
    --hPutStrLn stderr (showSDoc (vcat (map ppr mg2)))

    -- Now do the upsweep, calling compile for each module in
    -- turn.  Final result is version 3 of everything.

    -- Topologically sort the module graph, this time including hi-boot
    -- nodes, and possibly just including the portion of the graph
    -- reachable from the module specified in the 2nd argument to load.
    -- This graph should be cycle-free.
    -- If we're restricting the upsweep to a portion of the graph, we
    -- also want to retain everything that is still stable.
    let full_mg, partial_mg0, partial_mg, unstable_mg :: [SCC ModuleGraphNode]
        stable_mg :: [SCC ExtendedModSummary]
        full_mg    = topSortModuleGraph False mod_graph Nothing

        maybe_top_mod = case how_much of
                            LoadUpTo m           -> Just m
                            LoadDependenciesOf m -> Just m
                            _                    -> Nothing

        partial_mg0 = topSortModuleGraph False mod_graph maybe_top_mod

        -- LoadDependenciesOf m: we want the upsweep to stop just
        -- short of the specified module (unless the specified module
        -- is stable).
        partial_mg
            | LoadDependenciesOf _mod <- how_much
            = ASSERT( case last partial_mg0 of
                        AcyclicSCC (ModuleNode (ExtendedModSummary ms _)) -> ms_mod_name ms == _mod; _ -> False )
              List.init partial_mg0
            | otherwise
            = partial_mg0

        stable_mg =
            [ AcyclicSCC ems
            | AcyclicSCC (ModuleNode ems@(ExtendedModSummary ms _)) <- full_mg
            , stable_mod_summary ms
            ]

        stable_mod_summary ms =
          ms_mod_name ms `elementOfUniqSet` stable_obj ||
          ms_mod_name ms `elementOfUniqSet` stable_bco

        -- the modules from partial_mg that are not also stable
        -- NB. also keep cycles, we need to emit an error message later
        unstable_mg = filter not_stable partial_mg
          where not_stable (CyclicSCC _) = True
                not_stable (AcyclicSCC (InstantiationNode _)) = True
                not_stable (AcyclicSCC (ModuleNode (ExtendedModSummary ms _)))
                   = not $ stable_mod_summary ms

        -- Load all the stable modules first, before attempting to load
        -- an unstable module (#7231).
        mg = fmap (fmap ModuleNode) stable_mg ++ unstable_mg

    liftIO $ debugTraceMsg logger dflags 2 (hang (text "Ready for upsweep")
                               2 (ppr mg))

    n_jobs <- case parMakeCount dflags of
                    Nothing -> liftIO getNumProcessors
                    Just n  -> return n
    let upsweep_fn | n_jobs > 1 = parUpsweep n_jobs
                   | otherwise  = upsweep

    setSession hsc_env{ hsc_HPT = emptyHomePackageTable }
    (upsweep_ok, modsUpswept) <- withDeferredDiagnostics $
      upsweep_fn mHscMessage pruned_hpt stable_mods mg

    -- Make modsDone be the summaries for each home module now
    -- available; this should equal the domain of hpt3.
    -- Get in in a roughly top .. bottom order (hence reverse).

    let nodesDone = reverse modsUpswept
        (_, modsDone) = partitionNodes nodesDone

    -- Try and do linking in some form, depending on whether the
    -- upsweep was completely or only partially successful.

    if succeeded upsweep_ok

     then
       -- Easy; just relink it all.
       do liftIO $ debugTraceMsg logger dflags 2 (text "Upsweep completely successful.")

          -- Clean up after ourselves
          hsc_env1 <- getSession
          liftIO $ cleanCurrentModuleTempFiles logger (hsc_tmpfs hsc_env1) dflags

          -- Issue a warning for the confusing case where the user
          -- said '-o foo' but we're not going to do any linking.
          -- We attempt linking if either (a) one of the modules is
          -- called Main, or (b) the user said -no-hs-main, indicating
          -- that main() is going to come from somewhere else.
          --
          let ofile = outputFile dflags
          let no_hs_main = gopt Opt_NoHsMain dflags
          let
            main_mod = mainModIs hsc_env
            a_root_is_Main = mgElemModule mod_graph main_mod
            do_linking = a_root_is_Main || no_hs_main || ghcLink dflags == LinkDynLib || ghcLink dflags == LinkStaticLib

          -- link everything together
          hsc_env <- getSession
          linkresult <- liftIO $ link (ghcLink dflags)
                                      logger
                                      (hsc_tmpfs hsc_env)
                                      (hsc_hooks hsc_env)
                                      dflags
                                      (hsc_unit_env hsc_env)
                                      do_linking
                                      (hsc_HPT hsc_env1)

          if ghcLink dflags == LinkBinary && isJust ofile && not do_linking
             then do
                liftIO $ errorMsg logger dflags $ text
                   ("output was redirected with -o, " ++
                    "but no output will be generated\n" ++
                    "because there is no " ++
                    moduleNameString (moduleName main_mod) ++ " module.")
                -- This should be an error, not a warning (#10895).
                loadFinish Failed linkresult
             else
                loadFinish Succeeded linkresult

     else
       -- Tricky.  We need to back out the effects of compiling any
       -- half-done cycles, both so as to clean up the top level envs
       -- and to avoid telling the interactive linker to link them.
       do liftIO $ debugTraceMsg logger dflags 2 (text "Upsweep partially successful.")

          let modsDone_names
                 = map (ms_mod . emsModSummary) modsDone
          let mods_to_zap_names
                 = findPartiallyCompletedCycles modsDone_names
                      mg2_with_srcimps
          let (mods_to_clean, mods_to_keep) =
                partition ((`Set.member` mods_to_zap_names).ms_mod) $
                emsModSummary <$> modsDone
          hsc_env1 <- getSession
          let hpt4 = hsc_HPT hsc_env1
              -- We must change the lifetime to TFL_CurrentModule for any temp
              -- file created for an element of mod_to_clean during the upsweep.
              -- These include preprocessed files and object files for loaded
              -- modules.
              unneeded_temps = concat
                [ms_hspp_file : object_files
                | ModSummary{ms_mod, ms_hspp_file} <- mods_to_clean
                , let object_files = maybe [] linkableObjs $
                        lookupHpt hpt4 (moduleName ms_mod)
                        >>= hm_linkable
                ]
          tmpfs <- hsc_tmpfs <$> getSession
          liftIO $ changeTempFilesLifetime tmpfs TFL_CurrentModule unneeded_temps
          liftIO $ cleanCurrentModuleTempFiles logger tmpfs dflags

          let hpt5 = retainInTopLevelEnvs (map ms_mod_name mods_to_keep)
                                          hpt4

          -- Clean up after ourselves

          -- there should be no Nothings where linkables should be, now
          let just_linkables =
                    isNoLink (ghcLink dflags)
                 || allHpt (isJust.hm_linkable)
                        (filterHpt ((== HsSrcFile).mi_hsc_src.hm_iface)
                                hpt5)
          ASSERT( just_linkables ) do

          -- Link everything together
          hsc_env <- getSession
          linkresult <- liftIO $ link (ghcLink dflags)
                                      logger
                                      (hsc_tmpfs hsc_env)
                                      (hsc_hooks hsc_env)
                                      dflags
                                      (hsc_unit_env hsc_env)
                                      False
                                      hpt5

          modifySession $ \hsc_env -> hsc_env{ hsc_HPT = hpt5 }
          loadFinish Failed linkresult

partitionNodes
  :: [ModuleGraphNode]
  -> ( [InstantiatedUnit]
     , [ExtendedModSummary]
     )
partitionNodes ns = partitionEithers $ flip fmap ns $ \case
  InstantiationNode x -> Left x
  ModuleNode x -> Right x

-- | Finish up after a load.
loadFinish :: GhcMonad m => SuccessFlag -> SuccessFlag -> m SuccessFlag

-- If the link failed, unload everything and return.
loadFinish _all_ok Failed
  = do hsc_env <- getSession
       let interp = hscInterp hsc_env
       liftIO $ unload interp hsc_env []
       modifySession discardProg
       return Failed

-- Empty the interactive context and set the module context to the topmost
-- newly loaded module, or the Prelude if none were loaded.
loadFinish all_ok Succeeded
  = do modifySession discardIC
       return all_ok


-- | Forget the current program, but retain the persistent info in HscEnv
discardProg :: HscEnv -> HscEnv
discardProg hsc_env
  = discardIC $ hsc_env { hsc_mod_graph = emptyMG
                        , hsc_HPT = emptyHomePackageTable }

-- | Discard the contents of the InteractiveContext, but keep the DynFlags and
-- the loaded plugins.  It will also keep ic_int_print and ic_monad if their
-- names are from external packages.
discardIC :: HscEnv -> HscEnv
discardIC hsc_env
  = hsc_env { hsc_IC = empty_ic { ic_int_print = new_ic_int_print
                                , ic_monad     = new_ic_monad
                                , ic_plugins   = old_plugins
                                } }
  where
  -- Force the new values for ic_int_print and ic_monad to avoid leaking old_ic
  !new_ic_int_print = keep_external_name ic_int_print
  !new_ic_monad = keep_external_name ic_monad
  !old_plugins = ic_plugins old_ic
  dflags = ic_dflags old_ic
  old_ic = hsc_IC hsc_env
  empty_ic = emptyInteractiveContext dflags
  keep_external_name ic_name
    | nameIsFromExternalPackage home_unit old_name = old_name
    | otherwise = ic_name empty_ic
    where
    home_unit = hsc_home_unit hsc_env
    old_name = ic_name old_ic

-- | If there is no -o option, guess the name of target executable
-- by using top-level source file name as a base.
guessOutputFile :: GhcMonad m => m ()
guessOutputFile = modifySession $ \env ->
    let dflags = hsc_dflags env
        -- Force mod_graph to avoid leaking env
        !mod_graph = hsc_mod_graph env
        mainModuleSrcPath :: Maybe String
        mainModuleSrcPath = do
            ms <- mgLookupModule mod_graph (mainModIs env)
            ml_hs_file (ms_location ms)
        name = fmap dropExtension mainModuleSrcPath

        name_exe = do
#if defined(mingw32_HOST_OS)
          -- we must add the .exe extension unconditionally here, otherwise
          -- when name has an extension of its own, the .exe extension will
          -- not be added by GHC.Driver.Pipeline.exeFileName.  See #2248
          name' <- fmap (<.> "exe") name
#else
          name' <- name
#endif
          mainModuleSrcPath' <- mainModuleSrcPath
          -- #9930: don't clobber input files (unless they ask for it)
          if name' == mainModuleSrcPath'
            then throwGhcException . UsageError $
                 "default output name would overwrite the input file; " ++
                 "must specify -o explicitly"
            else Just name'
    in
    case outputFile_ dflags of
        Just _ -> env
        Nothing -> env { hsc_dflags = dflags { outputFile_ = name_exe } }

-- -----------------------------------------------------------------------------
--
-- | Return (names of) all those in modsDone who are part of a cycle as defined
-- by theGraph.
findPartiallyCompletedCycles :: [Module] -> [SCC ModSummary] -> Set.Set Module
findPartiallyCompletedCycles modsDone theGraph
   = Set.unions
       [mods_in_this_cycle
       | CyclicSCC vs <- theGraph  -- Acyclic? Not interesting.
       , let names_in_this_cycle = Set.fromList (map ms_mod vs)
             mods_in_this_cycle =
                    Set.intersection (Set.fromList modsDone) names_in_this_cycle
         -- If size mods_in_this_cycle == size names_in_this_cycle,
         -- then this cycle has already been completed and we're not
         -- interested.
       , Set.size mods_in_this_cycle < Set.size names_in_this_cycle]


-- ---------------------------------------------------------------------------
--
-- | Unloading
unload :: Interp -> HscEnv -> [Linkable] -> IO ()
unload interp hsc_env stable_linkables -- Unload everything *except* 'stable_linkables'
  = case ghcLink (hsc_dflags hsc_env) of
        LinkInMemory -> Linker.unload interp hsc_env stable_linkables
        _other -> return ()

-- -----------------------------------------------------------------------------
{- |

  Stability tells us which modules definitely do not need to be recompiled.
  There are two main reasons for having stability:

   - avoid doing a complete upsweep of the module graph in GHCi when
     modules near the bottom of the tree have not changed.

   - to tell GHCi when it can load object code: we can only load object code
     for a module when we also load object code fo  all of the imports of the
     module.  So we need to know that we will definitely not be recompiling
     any of these modules, and we can use the object code.

  The stability check is as follows.  Both stableObject and
  stableBCO are used during the upsweep phase later.

@
  stable m = stableObject m || stableBCO m

  
  stableObject m =
        all stableObject (imports m) 
        -- * a module is stable if all importing modules are stable 
        && old linkable does not exist, or is == on-disk .o 
        -- ! shouldn't this be : old linkable exists 
        && date(on-disk .o) > date(.hs) 
        -- * the complied version of the module is newer than the source file
  
  -- * byte-code objects (BCOs) produced for the GHC’s byte-code interpreter
  stableBCO m =
        -- * a module is stable if all importing modules are stable
        all stable (imports m)
        -- * the compiled version of the module is newer than the source file
        && date(BCO) > date(.hs)
@

  These properties embody the following ideas:

    - if a module is stable, then:

        - if it has been compiled in a previous pass (present in HPT)
          then it does not need to be compiled or re-linked.

        - if it has not been compiled in a previous pass,
          then we only need to read its .hi file from disk and
          link it to produce a 'ModDetails'.

    - if a modules is not stable, we will definitely be at least
      re-linking, and possibly re-compiling it during the 'upsweep'.
      All non-stable modules can (and should) therefore be unlinked
      before the 'upsweep'.

    - Note that objects are only considered stable if they only depend
      on other objects.  We can't link object code against byte code.

    - Note that even if an object is stable, we may end up recompiling
      if the interface is out of date because an *external* interface
      has changed.  The current code in GHC.Driver.Make handles this case
      fairly poorly, so be careful.
-}

type StableModules =
  ( UniqSet ModuleName  -- stableObject
  , UniqSet ModuleName  -- stableBCO
  )


checkStability
        :: HomePackageTable   -- HPT from last compilation
        -> [SCC ModSummary]   -- current module graph (cyclic)
        -> UniqSet ModuleName -- all home modules
        -> StableModules

checkStability hpt sccs all_home_mods =
  foldl' checkSCC (emptyUniqSet, emptyUniqSet) sccs
  where
   checkSCC :: StableModules -> SCC ModSummary -> StableModules
   checkSCC (stable_obj, stable_bco) scc0
     | stableObjects = (addListToUniqSet stable_obj scc_mods, stable_bco)
     | stableBCOs    = (stable_obj, addListToUniqSet stable_bco scc_mods)
     | otherwise     = (stable_obj, stable_bco)
     where
        scc = flattenSCC scc0
        scc_mods = map ms_mod_name scc
        home_module m =
          m `elementOfUniqSet` all_home_mods && m `notElem` scc_mods

        scc_allimps = nub (filter home_module (concatMap ms_home_allimps scc))
            -- all imports outside the current SCC, but in the home pkg

        stable_obj_imps = map (`elementOfUniqSet` stable_obj) scc_allimps
        stable_bco_imps = map (`elementOfUniqSet` stable_bco) scc_allimps

        stableObjects =
           and stable_obj_imps
           && all object_ok scc

        stableBCOs =
           and (zipWith (||) stable_obj_imps stable_bco_imps)
           && all bco_ok scc

        object_ok ms
          | gopt Opt_ForceRecomp (ms_hspp_opts ms) = False
          | Just t <- ms_obj_date ms  =  t >= ms_hs_date ms
                                         && same_as_prev t
          | otherwise = False
          where
             same_as_prev t = case lookupHpt hpt (ms_mod_name ms) of
                                Just hmi  | Just l <- hm_linkable hmi
                                 -> isObjectLinkable l && t == linkableTime l
                                _other  -> True
                -- why '>=' rather than '>' above?  If the filesystem stores
                -- times to the nearest second, we may occasionally find that
                -- the object & source have the same modification time,
                -- especially if the source was automatically generated
                -- and compiled.  Using >= is slightly unsafe, but it matches
                -- make's behaviour.
                --
                -- But see #5527, where someone ran into this and it caused
                -- a problem.

        bco_ok ms
          | gopt Opt_ForceRecomp (ms_hspp_opts ms) = False
          | otherwise = case lookupHpt hpt (ms_mod_name ms) of
                Just hmi  | Just l <- hm_linkable hmi ->
                        not (isObjectLinkable l) &&
                        linkableTime l >= ms_hs_date ms
                _other  -> False

{- Parallel Upsweep
 -
 - The parallel upsweep attempts to concurrently compile the modules in the
 - compilation graph using multiple Haskell threads.
 -
 - The Algorithm
 -
 - A Haskell thread is spawned for each module in the module graph, waiting for
 - its direct dependencies to finish building before it itself begins to build.
 -
 - Each module is associated with an initially empty MVar that stores the
 - result of that particular module's compile. If the compile succeeded, then
 - the HscEnv (synchronized by an MVar) is updated with the fresh HMI of that
 - module, and the module's HMI is deleted from the old HPT (synchronized by an
 - IORef) to save space.
 -
 - Instead of immediately outputting messages to the standard handles, all
 - compilation output is deferred to a per-module TQueue. A QSem is used to
 - limit the number of workers that are compiling simultaneously.
 -
 - Meanwhile, the main thread sequentially loops over all the modules in the
 - module graph, outputting the messages stored in each module's TQueue.
-}

-- | Each module is given a unique 'LogQueue' to redirect compilation messages
-- to. A 'Nothing' value contains the result of compilation, and denotes the
-- end of the message queue.
data LogQueue = LogQueue !(IORef [Maybe (WarnReason, Severity, SrcSpan, SDoc)])
                         !(MVar ())

-- | The graph of modules to compile and their corresponding result 'MVar' and
-- 'LogQueue'.
type CompilationGraph = [(ModuleGraphNode, MVar SuccessFlag, LogQueue)]

-- | Build a 'CompilationGraph' out of a list of strongly-connected modules,
-- also returning the first, if any, encountered module cycle.
buildCompGraph :: [SCC ModuleGraphNode] -> IO (CompilationGraph, Maybe [ModuleGraphNode])
buildCompGraph [] = return ([], Nothing)
buildCompGraph (scc:sccs) = case scc of
    AcyclicSCC ms -> do
        mvar <- newEmptyMVar
        log_queue <- do
            ref <- newIORef []
            sem <- newEmptyMVar
            return (LogQueue ref sem)
        (rest,cycle) <- buildCompGraph sccs
        return ((ms,mvar,log_queue):rest, cycle)
    CyclicSCC mss -> return ([], Just mss)

-- | A Module and whether it is a boot module.
--
-- We need to treat boot modules specially when building compilation graphs,
-- since they break cycles. Regular source files and signature files are treated
-- equivalently.
data BuildModule = BuildModule_Unit {-# UNPACK #-} !InstantiatedUnit | BuildModule_Module {-# UNPACK #-} !ModuleWithIsBoot
  deriving (Eq, Ord)

-- | Tests if an 'HscSource' is a boot file, primarily for constructing elements
-- of 'BuildModule'. We conflate signatures and modules because they are bound
-- in the same namespace; only boot interfaces can be disambiguated with
-- `import {-# SOURCE #-}`.
hscSourceToIsBoot :: HscSource -> IsBootInterface
hscSourceToIsBoot HsBootFile = IsBoot
hscSourceToIsBoot _ = NotBoot

mkBuildModule :: ModuleGraphNode -> BuildModule
mkBuildModule = \case
  InstantiationNode x -> BuildModule_Unit x
  ModuleNode ems -> BuildModule_Module $ mkBuildModule0 (emsModSummary ems)

mkHomeBuildModule :: ModuleGraphNode -> NodeKey
mkHomeBuildModule = \case
  InstantiationNode x -> NodeKey_Unit x
  ModuleNode ems -> NodeKey_Module $ mkHomeBuildModule0 (emsModSummary ems)

mkBuildModule0 :: ModSummary -> ModuleWithIsBoot
mkBuildModule0 ms = GWIB
  { gwib_mod = ms_mod ms
  , gwib_isBoot = isBootSummary ms
  }

mkHomeBuildModule0 :: ModSummary -> ModuleNameWithIsBoot
mkHomeBuildModule0 ms = GWIB
  { gwib_mod = moduleName $ ms_mod ms
  , gwib_isBoot = isBootSummary ms
  }

-- | The entry point to the parallel upsweep.
--
-- See also the simpler, sequential 'upsweep'.
parUpsweep
    :: GhcMonad m
    => Int
    -- ^ The number of workers we wish to run in parallel
    -> Maybe Messager
    -> HomePackageTable
    -> StableModules
    -> [SCC ModuleGraphNode]
    -> m (SuccessFlag,
          [ModuleGraphNode])
parUpsweep n_jobs mHscMessage old_hpt stable_mods sccs = do
    hsc_env <- getSession
    let dflags = hsc_dflags hsc_env
    let logger = hsc_logger hsc_env
    let tmpfs  = hsc_tmpfs hsc_env

    -- The bits of shared state we'll be using:

    -- The global HscEnv is updated with the module's HMI when a module
    -- successfully compiles.
    hsc_env_var <- liftIO $ newMVar hsc_env

    -- The old HPT is used for recompilation checking in upsweep_mod. When a
    -- module successfully gets compiled, its HMI is pruned from the old HPT.
    old_hpt_var <- liftIO $ newIORef old_hpt

    -- What we use to limit parallelism with.
    par_sem <- liftIO $ newQSem n_jobs


    let updNumCapabilities = liftIO $ do
            n_capabilities <- getNumCapabilities
            n_cpus <- getNumProcessors
            -- Setting number of capabilities more than
            -- CPU count usually leads to high userspace
            -- lock contention. #9221
            let n_caps = min n_jobs n_cpus
            unless (n_capabilities /= 1) $ setNumCapabilities n_caps
            return n_capabilities
    -- Reset the number of capabilities once the upsweep ends.
    let resetNumCapabilities orig_n = liftIO $ setNumCapabilities orig_n

    MC.bracket updNumCapabilities resetNumCapabilities $ \_ -> do

    -- Sync the global session with the latest HscEnv once the upsweep ends.
    let finallySyncSession io = io `MC.finally` do
            hsc_env <- liftIO $ readMVar hsc_env_var
            setSession hsc_env

    finallySyncSession $ do

    -- Build the compilation graph out of the list of SCCs. Module cycles are
    -- handled at the very end, after some useful work gets done. Note that
    -- this list is topologically sorted (by virtue of 'sccs' being sorted so).
    (comp_graph,cycle) <- liftIO $ buildCompGraph sccs
    let comp_graph_w_idx = zip comp_graph [1..]

    -- The list of all loops in the compilation graph.
    -- NB: For convenience, the last module of each loop (aka the module that
    -- finishes the loop) is prepended to the beginning of the loop.
    let graph = map fstOf3 (reverse comp_graph)
        boot_modules = mkModuleSet
          [ms_mod ms | ModuleNode (ExtendedModSummary ms _) <- graph, isBootSummary ms == IsBoot]
        comp_graph_loops = go graph boot_modules
          where
            remove ms bm = case isBootSummary ms of
              IsBoot -> delModuleSet bm (ms_mod ms)
              NotBoot -> bm
            go [] _ = []
            go (InstantiationNode _ : mss) boot_modules
              = go mss boot_modules
            go mg@(mnode@(ModuleNode (ExtendedModSummary ms _)) : mss) boot_modules
              | Just loop <- getModLoop ms mg (`elemModuleSet` boot_modules)
              = map mkBuildModule (mnode : loop) : go mss (remove ms boot_modules)
              | otherwise
              = go mss (remove ms boot_modules)

    -- Build a Map out of the compilation graph with which we can efficiently
    -- look up the result MVar associated with a particular home module.
    let home_mod_map :: Map BuildModule (MVar SuccessFlag, Int)
        home_mod_map =
            Map.fromList [ (mkBuildModule ms, (mvar, idx))
                         | ((ms,mvar,_),idx) <- comp_graph_w_idx ]


    liftIO $ label_self "main --make thread"

    -- Make the logger thread_safe: we only make the "log" action thread-safe in
    -- each worker by setting a LogAction hook, so we need to make the logger
    -- thread-safe for other actions (DumpAction, TraceAction).
    thread_safe_logger <- liftIO $ makeThreadSafe logger

    -- For each module in the module graph, spawn a worker thread that will
    -- compile this module.
    let { spawnWorkers = forM comp_graph_w_idx $ \((mod,!mvar,!log_queue),!mod_idx) ->
            forkIOWithUnmask $ \unmask -> do
                liftIO $ label_self $ unwords $ concat
                    [ [ "worker --make thread" ]
                    , case mod of
                        InstantiationNode iuid ->
                          [ "for instantiation of unit"
                          , show $ VirtUnit iuid
                          ]
                        ModuleNode ems ->
                          [ "for module"
                          , show (moduleNameString (ms_mod_name (emsModSummary ems)))
                          ]
                    , ["number"
                      , show mod_idx
                      ]
                    ]
                -- Replace the default log_action with one that writes each
                -- message to the module's log_queue. The main thread will
                -- deal with synchronously printing these messages.
                let lcl_logger = pushLogHook (const (parLogAction log_queue)) thread_safe_logger

                -- Use a local TmpFs so that we can clean up intermediate files
                -- in a timely fashion (as soon as compilation for that module
                -- is finished) without having to worry about accidentally
                -- deleting a simultaneous compile's important files.
                lcl_tmpfs <- forkTmpFsFrom tmpfs

                -- Unmask asynchronous exceptions and perform the thread-local
                -- work to compile the module (see parUpsweep_one).
                m_res <- MC.try $ unmask $ prettyPrintGhcErrors dflags $
                  case mod of
                    InstantiationNode iuid -> do
                      hsc_env <- readMVar hsc_env_var
                      liftIO $ upsweep_inst hsc_env mHscMessage mod_idx (length sccs) iuid
                      pure Succeeded
                    ModuleNode ems ->
                      parUpsweep_one (emsModSummary ems) home_mod_map comp_graph_loops
                                     lcl_logger lcl_tmpfs dflags (hsc_home_unit hsc_env)
                                     mHscMessage
                                     par_sem hsc_env_var old_hpt_var
                                     stable_mods mod_idx (length sccs)

                res <- case m_res of
                    Right flag -> return flag
                    Left exc -> do
                        -- Don't print ThreadKilled exceptions: they are used
                        -- to kill the worker thread in the event of a user
                        -- interrupt, and the user doesn't have to be informed
                        -- about that.
                        when (fromException exc /= Just ThreadKilled)
                             (errorMsg lcl_logger dflags (text (show exc)))
                        return Failed

                -- Populate the result MVar.
                putMVar mvar res

                -- Write the end marker to the message queue, telling the main
                -- thread that it can stop waiting for messages from this
                -- particular compile.
                writeLogQueue log_queue Nothing

                -- Add the remaining files that weren't cleaned up to the
                -- global TmpFs, for cleanup later.
                mergeTmpFsInto lcl_tmpfs tmpfs

        -- Kill all the workers, masking interrupts (since killThread is
        -- interruptible). XXX: This is not ideal.
        ; killWorkers = MC.uninterruptibleMask_ . mapM_ killThread }


    -- Spawn the workers, making sure to kill them later. Collect the results
    -- of each compile.
    results <- liftIO $ MC.bracket spawnWorkers killWorkers $ \_ ->
        -- Loop over each module in the compilation graph in order, printing
        -- each message from its log_queue.
        forM comp_graph $ \(mod,mvar,log_queue) -> do
            printLogs logger dflags log_queue
            result <- readMVar mvar
            if succeeded result then return (Just mod) else return Nothing


    -- Collect and return the ModSummaries of all the successful compiles.
    -- NB: Reverse this list to maintain output parity with the sequential upsweep.
    let ok_results = reverse (catMaybes results)

    -- Handle any cycle in the original compilation graph and return the result
    -- of the upsweep.
    case cycle of
        Just mss -> do
            liftIO $ fatalErrorMsg logger dflags (cyclicModuleErr mss)
            return (Failed,ok_results)
        Nothing  -> do
            let success_flag = successIf (all isJust results)
            return (success_flag,ok_results)

  where
    writeLogQueue :: LogQueue -> Maybe (WarnReason,Severity,SrcSpan,SDoc) -> IO ()
    writeLogQueue (LogQueue ref sem) msg = do
        atomicModifyIORef' ref $ \msgs -> (msg:msgs,())
        _ <- tryPutMVar sem ()
        return ()

    -- The log_action callback that is used to synchronize messages from a
    -- worker thread.
    parLogAction :: LogQueue -> LogAction
    parLogAction log_queue _dflags !reason !severity !srcSpan !msg =
        writeLogQueue log_queue (Just (reason,severity,srcSpan,msg))

    -- Print each message from the log_queue using the log_action from the
    -- session's DynFlags.
    printLogs :: Logger -> DynFlags -> LogQueue -> IO ()
    printLogs !logger !dflags (LogQueue ref sem) = read_msgs
      where read_msgs = do
                takeMVar sem
                msgs <- atomicModifyIORef' ref $ \xs -> ([], reverse xs)
                print_loop msgs

            print_loop [] = read_msgs
            print_loop (x:xs) = case x of
                Just (reason,severity,srcSpan,msg) -> do
                    putLogMsg logger dflags reason severity srcSpan msg
                    print_loop xs
                -- Exit the loop once we encounter the end marker.
                Nothing -> return ()

-- The interruptible subset of the worker threads' work.
parUpsweep_one
    :: ModSummary
    -- ^ The module we wish to compile
    -> Map BuildModule (MVar SuccessFlag, Int)
    -- ^ The map of home modules and their result MVar
    -> [[BuildModule]]
    -- ^ The list of all module loops within the compilation graph.
    -> Logger
    -- ^ The thread-local Logger
    -> TmpFs
    -- ^ The thread-local TmpFs
    -> DynFlags
    -- ^ The thread-local DynFlags
    -> HomeUnit
    -- ^ The home-unit
    -> Maybe Messager
    -- ^ The messager
    -> QSem
    -- ^ The semaphore for limiting the number of simultaneous compiles
    -> MVar HscEnv
    -- ^ The MVar that synchronizes updates to the global HscEnv
    -> IORef HomePackageTable
    -- ^ The old HPT
    -> StableModules
    -- ^ Sets of stable objects and BCOs
    -> Int
    -- ^ The index of this module
    -> Int
    -- ^ The total number of modules
    -> IO SuccessFlag
    -- ^ The result of this compile
parUpsweep_one mod home_mod_map comp_graph_loops lcl_logger lcl_tmpfs lcl_dflags home_unit mHscMessage par_sem
               hsc_env_var old_hpt_var stable_mods mod_index num_mods = do

    let this_build_mod = mkBuildModule0 mod

    let home_imps     = map unLoc $ ms_home_imps mod
    let home_src_imps = map unLoc $ ms_home_srcimps mod

    -- All the textual imports of this module.
    let textual_deps = Set.fromList $
            zipWith f home_imps     (repeat NotBoot) ++
            zipWith f home_src_imps (repeat IsBoot)
          where f mn isBoot = BuildModule_Module $ GWIB
                  { gwib_mod = mkHomeModule home_unit mn
                  , gwib_isBoot = isBoot
                  }

    -- Dealing with module loops
    -- ~~~~~~~~~~~~~~~~~~~~~~~~~
    --
    -- Not only do we have to deal with explicit textual dependencies, we also
    -- have to deal with implicit dependencies introduced by import cycles that
    -- are broken by an hs-boot file. We have to ensure that:
    --
    -- 1. A module that breaks a loop must depend on all the modules in the
    --    loop (transitively or otherwise). This is normally always fulfilled
    --    by the module's textual dependencies except in degenerate loops,
    --    e.g.:
    --
    --    A.hs imports B.hs-boot
    --    B.hs doesn't import A.hs
    --    C.hs imports A.hs, B.hs
    --
    --    In this scenario, getModLoop will detect the module loop [A,B] but
    --    the loop finisher B doesn't depend on A. So we have to explicitly add
    --    A in as a dependency of B when we are compiling B.
    --
    -- 2. A module that depends on a module in an external loop can't proceed
    --    until the entire loop is re-typechecked.
    --
    -- These two invariants have to be maintained to correctly build a
    -- compilation graph with one or more loops.


    -- The loop that this module will finish. After this module successfully
    -- compiles, this loop is going to get re-typechecked.
    let finish_loop :: Maybe [ModuleWithIsBoot]
        finish_loop = listToMaybe
          [ flip mapMaybe (tail loop) $ \case
              BuildModule_Unit _ -> Nothing
              BuildModule_Module ms -> Just ms
          | loop <- comp_graph_loops
          , head loop == BuildModule_Module this_build_mod
          ]

    -- If this module finishes a loop then it must depend on all the other
    -- modules in that loop because the entire module loop is going to be
    -- re-typechecked once this module gets compiled. These extra dependencies
    -- are this module's "internal" loop dependencies, because this module is
    -- inside the loop in question.
    let int_loop_deps :: Set.Set BuildModule
        int_loop_deps = Set.fromList $
            case finish_loop of
                Nothing   -> []
                Just loop -> BuildModule_Module <$> filter (/= this_build_mod) loop

    -- If this module depends on a module within a loop then it must wait for
    -- that loop to get re-typechecked, i.e. it must wait on the module that
    -- finishes that loop. These extra dependencies are this module's
    -- "external" loop dependencies, because this module is outside of the
    -- loop(s) in question.
    let ext_loop_deps :: Set.Set BuildModule
        ext_loop_deps = Set.fromList
            [ head loop | loop <- comp_graph_loops
                        , any (`Set.member` textual_deps) loop
                        , BuildModule_Module this_build_mod `notElem` loop ]


    let all_deps = foldl1 Set.union [textual_deps, int_loop_deps, ext_loop_deps]

    -- All of the module's home-module dependencies.
    let home_deps_with_idx =
            [ home_dep | dep <- Set.toList all_deps
                       , Just home_dep <- [Map.lookup dep home_mod_map]
                       ]

    -- Sort the list of dependencies in reverse-topological order. This way, by
    -- the time we get woken up by the result of an earlier dependency,
    -- subsequent dependencies are more likely to have finished. This step
    -- effectively reduces the number of MVars that each thread blocks on.
    let home_deps = map fst $ sortBy (flip (comparing snd)) home_deps_with_idx

    -- Wait for the all the module's dependencies to finish building.
    deps_ok <- allM (fmap succeeded . readMVar) home_deps

    -- We can't build this module if any of its dependencies failed to build.
    if not deps_ok
      then return Failed
      else do
        -- Any hsc_env at this point is OK to use since we only really require
        -- that the HPT contains the HMIs of our dependencies.
        hsc_env <- readMVar hsc_env_var
        old_hpt <- readIORef old_hpt_var

        let logg err = printBagOfErrors lcl_logger lcl_dflags (srcErrorMessages err)

        -- Limit the number of parallel compiles.
        let withSem sem = MC.bracket_ (waitQSem sem) (signalQSem sem)
        mb_mod_info <- withSem par_sem $
            handleSourceError (\err -> do logg err; return Nothing) $ do
                -- Have the HscEnv point to our local logger and tmpfs.
                let lcl_hsc_env = localize_hsc_env hsc_env

                -- Re-typecheck the loop
                -- This is necessary to make sure the knot is tied when
                -- we close a recursive module loop, see bug #12035.
                type_env_var <- liftIO $ newIORef emptyNameEnv
                let lcl_hsc_env' = lcl_hsc_env { hsc_type_env_var =
                                    Just (ms_mod mod, type_env_var) }
                lcl_hsc_env'' <- case finish_loop of
                    Nothing   -> return lcl_hsc_env'
                    -- In the non-parallel case, the retypecheck prior to
                    -- typechecking the loop closer includes all modules
                    -- EXCEPT the loop closer.  However, our precomputed
                    -- SCCs include the loop closer, so we have to filter
                    -- it out.
                    Just loop -> typecheckLoop lcl_dflags lcl_hsc_env' $
                                 filter (/= moduleName (gwib_mod this_build_mod)) $
                                 map (moduleName . gwib_mod) loop

                -- Compile the module.
                mod_info <- upsweep_mod lcl_hsc_env'' mHscMessage old_hpt stable_mods
                                        mod mod_index num_mods
                return (Just mod_info)

        case mb_mod_info of
            Nothing -> return Failed
            Just mod_info -> do
                let this_mod = ms_mod_name mod

                -- Prune the old HPT unless this is an hs-boot module.
                unless (isBootSummary mod == IsBoot) $
                    atomicModifyIORef' old_hpt_var $ \old_hpt ->
                        (delFromHpt old_hpt this_mod, ())

                -- Update and fetch the global HscEnv.
                lcl_hsc_env' <- modifyMVar hsc_env_var $ \hsc_env -> do
                    let hsc_env' = hsc_env
                                     { hsc_HPT = addToHpt (hsc_HPT hsc_env)
                                                           this_mod mod_info }
                    -- We've finished typechecking the module, now we must
                    -- retypecheck the loop AGAIN to ensure unfoldings are
                    -- updated.  This time, however, we include the loop
                    -- closer!
                    hsc_env'' <- case finish_loop of
                        Nothing   -> return hsc_env'
                        Just loop -> typecheckLoop lcl_dflags hsc_env' $
                                     map (moduleName . gwib_mod) loop
                    return (hsc_env'', localize_hsc_env hsc_env'')

                -- Clean up any intermediate files.
                cleanCurrentModuleTempFiles (hsc_logger lcl_hsc_env')
                                            (hsc_tmpfs  lcl_hsc_env')
                                            (hsc_dflags lcl_hsc_env')
                return Succeeded

  where
    localize_hsc_env hsc_env
        = hsc_env { hsc_logger = lcl_logger
                  , hsc_tmpfs  = lcl_tmpfs
                  }

-- -----------------------------------------------------------------------------
--
-- | The upsweep
--
-- This is where we compile each module in the module graph, in a pass
-- from the bottom to the top of the graph.
--
-- There better had not be any cyclic groups here -- we check for them.
upsweep
    :: forall m
    .  GhcMonad m
    => Maybe Messager
    -> HomePackageTable            -- ^ HPT from last time round (pruned)
    -> StableModules               -- ^ stable modules (see checkStability)
    -> [SCC ModuleGraphNode]       -- ^ Mods to do (the worklist)
    -> m (SuccessFlag,
          [ModuleGraphNode])
       -- ^ Returns:
       --
       --  1. A flag whether the complete upsweep was successful.
       --  2. The 'HscEnv' in the monad has an updated HPT
       --  3. A list of modules which succeeded loading.

upsweep mHscMessage old_hpt stable_mods sccs = do
   (res, done) <- upsweep' old_hpt emptyMG sccs 1 (length sccs)
   return (res, reverse $ mgModSummaries' done)
 where
  keep_going
    :: [NodeKey]
    -> HomePackageTable
    -> ModuleGraph
    -> [SCC ModuleGraphNode]
    -> Int
    -> Int
    -> m (SuccessFlag, ModuleGraph)
  keep_going this_mods old_hpt done mods mod_index nmods = do
    let sum_deps ms (AcyclicSCC iuidOrMod) =
          if any (flip elem $ unfilteredEdges False iuidOrMod) $ ms
          then mkHomeBuildModule iuidOrMod : ms
          else ms
        sum_deps ms _ = ms
        dep_closure = foldl' sum_deps this_mods mods
        dropped_ms = drop (length this_mods) (reverse dep_closure)
        prunable (AcyclicSCC node) = elem (mkHomeBuildModule node) dep_closure
        prunable _ = False
        mods' = filter (not . prunable) mods
        nmods' = nmods - length dropped_ms

    when (not $ null dropped_ms) $ do
        dflags <- getSessionDynFlags
        logger <- getLogger
        liftIO $ fatalErrorMsg logger dflags (keepGoingPruneErr $ dropped_ms)
    (_, done') <- upsweep' old_hpt done mods' (mod_index+1) nmods'
    return (Failed, done')

  upsweep'
    :: HomePackageTable
    -> ModuleGraph
    -> [SCC ModuleGraphNode]
    -> Int
    -> Int
    -> m (SuccessFlag, ModuleGraph)
  upsweep' _old_hpt done
     [] _ _
     = return (Succeeded, done)

  upsweep' _old_hpt done
     (CyclicSCC ms : mods) mod_index nmods
   = do dflags <- getSessionDynFlags
        logger <- getLogger
        liftIO $ fatalErrorMsg logger dflags (cyclicModuleErr ms)
        if gopt Opt_KeepGoing dflags
          then keep_going (mkHomeBuildModule <$> ms) old_hpt done mods mod_index nmods
          else return (Failed, done)

  upsweep' old_hpt done
     (AcyclicSCC (InstantiationNode iuid) : mods) mod_index nmods
   = do hsc_env <- getSession
        liftIO $ upsweep_inst hsc_env mHscMessage mod_index nmods iuid
        upsweep' old_hpt done mods (mod_index+1) nmods

  upsweep' old_hpt done
     (AcyclicSCC (ModuleNode ems@(ExtendedModSummary mod _)) : mods) mod_index nmods
   = do -- putStrLn ("UPSWEEP_MOD: hpt = " ++
        --           show (map (moduleUserString.moduleName.mi_module.hm_iface)
        --                     (moduleEnvElts (hsc_HPT hsc_env)))
        let logg _mod = defaultWarnErrLogger

        hsc_env <- getSession

        -- Remove unwanted tmp files between compilations
        liftIO $ cleanCurrentModuleTempFiles (hsc_logger hsc_env)
                                             (hsc_tmpfs  hsc_env)
                                             (hsc_dflags hsc_env)

        -- Get ready to tie the knot
        type_env_var <- liftIO $ newIORef emptyNameEnv
        let hsc_env1 = hsc_env { hsc_type_env_var =
                                    Just (ms_mod mod, type_env_var) }
        setSession hsc_env1

        -- Lazily reload the HPT modules participating in the loop.
        -- See Note [Tying the knot]--if we don't throw out the old HPT
        -- and reinitalize the knot-tying process, anything that was forced
        -- while we were previously typechecking won't get updated, this
        -- was bug #12035.
        hsc_env2 <- liftIO $ reTypecheckLoop hsc_env1 mod done
        setSession hsc_env2

        mb_mod_info
            <- handleSourceError
                   (\err -> do logg mod (Just err); return Nothing) $ do
                 mod_info <- liftIO $ upsweep_mod hsc_env2 mHscMessage old_hpt stable_mods
                                                  mod mod_index nmods
                 logg mod Nothing -- log warnings
                 return (Just mod_info)

        case mb_mod_info of
          Nothing -> do
                dflags <- getSessionDynFlags
                if gopt Opt_KeepGoing dflags
                  then keep_going [NodeKey_Module $ mkHomeBuildModule0 mod] old_hpt done mods mod_index nmods
                  else return (Failed, done)
          Just mod_info -> do
                let this_mod = ms_mod_name mod

                        -- Add new info to hsc_env
                    hpt1     = addToHpt (hsc_HPT hsc_env2) this_mod mod_info
                    hsc_env3 = hsc_env2 { hsc_HPT = hpt1, hsc_type_env_var = Nothing }

                        -- Space-saving: delete the old HPT entry
                        -- for mod BUT if mod is a hs-boot
                        -- node, don't delete it.  For the
                        -- interface, the HPT entry is probably for the
                        -- main Haskell source file.  Deleting it
                        -- would force the real module to be recompiled
                        -- every time.
                    old_hpt1 = case isBootSummary mod of
                      IsBoot -> old_hpt
                      NotBoot -> delFromHpt old_hpt this_mod

                    done' = extendMG done ems

                        -- fixup our HomePackageTable after we've finished compiling
                        -- a mutually-recursive loop.  We have to do this again
                        -- to make sure we have the final unfoldings, which may
                        -- not have been computed accurately in the previous
                        -- retypecheck.
                hsc_env4 <- liftIO $ reTypecheckLoop hsc_env3 mod done'
                setSession hsc_env4

                        -- Add any necessary entries to the static pointer
                        -- table. See Note [Grand plan for static forms] in
                        -- GHC.Iface.Tidy.StaticPtrTable.
                when (backend (hsc_dflags hsc_env4) == Interpreter) $
                    liftIO $ hscAddSptEntries hsc_env4
                                 [ spt
                                 | Just linkable <- pure $ hm_linkable mod_info
                                 , unlinked <- linkableUnlinked linkable
                                 , BCOs _ spts <- pure unlinked
                                 , spt <- spts
                                 ]

                upsweep' old_hpt1 done' mods (mod_index+1) nmods

-- maybeGetIfaceDate :: DynFlags -> ModLocation -> IO (Maybe UTCTime)
-- maybeGetIfaceDate dflags location
--  | writeInterfaceOnlyMode dflags
--     -- Minor optimization: it should be harmless to check the hi file location
--     -- always, but it's better to avoid hitting the filesystem if possible.
--     = modificationTimeIfExists (ml_hi_file location)
--  | otherwise
--     = return Nothing

upsweep_inst :: HscEnv
             -> Maybe Messager
             -> Int  -- index of module
             -> Int  -- total number of modules
             -> InstantiatedUnit
             -> IO ()
upsweep_inst hsc_env mHscMessage mod_index nmods iuid = do
        case mHscMessage of
            Just hscMessage -> hscMessage hsc_env (mod_index, nmods) MustCompile (InstantiationNode iuid)
            Nothing -> return ()
        runHsc hsc_env $ ioMsgMaybe $ tcRnCheckUnit hsc_env $ VirtUnit iuid
        pure ()

-- | Compile a single module.  Always produce a Linkable for it if
-- successful.  If no compilation happened, return the old Linkable.
upsweep_mod :: HscEnv
            -> Maybe Messager
            -> HomePackageTable
            -> StableModules
            -> ModSummary
            -> Int  -- index of module
            -> Int  -- total number of modules
            -> IO HomeModInfo
upsweep_mod hsc_env mHscMessage old_hpt (stable_obj, stable_bco) summary mod_index nmods
   =    let
            this_mod_name = ms_mod_name summary
            this_mod    = ms_mod summary
            mb_obj_date = ms_obj_date summary
            mb_if_date  = ms_iface_date summary
            obj_fn      = ml_obj_file (ms_location summary)
            hs_date     = ms_hs_date summary

            is_stable_obj = this_mod_name `elementOfUniqSet` stable_obj
            is_stable_bco = this_mod_name `elementOfUniqSet` stable_bco

            old_hmi = lookupHpt old_hpt this_mod_name

            -- We're using the dflags for this module now, obtained by
            -- applying any options in its LANGUAGE & OPTIONS_GHC pragmas.
            lcl_dflags = ms_hspp_opts summary
            prevailing_backend = backend (hsc_dflags hsc_env)
            local_backend      = backend lcl_dflags

            -- If OPTIONS_GHC contains -fasm or -fllvm, be careful that
            -- we don't do anything dodgy: these should only work to change
            -- from -fllvm to -fasm and vice-versa, or away from -fno-code,
            -- otherwise we could end up trying to link object code to byte
            -- code.
            bcknd = case (prevailing_backend,local_backend) of
               (LLVM,NCG) -> NCG
               (NCG,LLVM) -> LLVM
               (NoBackend,b)
                  | backendProducesObject b -> b
               (Interpreter,b)
                  | backendProducesObject b -> b
               _ -> prevailing_backend

            -- store the corrected backend into the summary
            summary' = summary{ ms_hspp_opts = lcl_dflags { backend = bcknd } }

            -- The old interface is ok if
            --  a) we're compiling a source file, and the old HPT
            --     entry is for a source file
            --  b) we're compiling a hs-boot file
            -- Case (b) allows an hs-boot file to get the interface of its
            -- real source file on the second iteration of the compilation
            -- manager, but that does no harm.  Otherwise the hs-boot file
            -- will always be recompiled

            mb_old_iface
                = case old_hmi of
                     Nothing                                        -> Nothing
                     Just hm_info | isBootSummary summary == IsBoot -> Just iface
                                  | mi_boot iface == NotBoot        -> Just iface
                                  | otherwise                       -> Nothing
                                   where
                                     iface = hm_iface hm_info

            compile_it :: Maybe Linkable -> SourceModified -> IO HomeModInfo
            compile_it  mb_linkable src_modified =
                  compileOne' Nothing mHscMessage hsc_env summary' mod_index nmods
                             mb_old_iface mb_linkable src_modified

            compile_it_discard_iface :: Maybe Linkable -> SourceModified
                                     -> IO HomeModInfo
            compile_it_discard_iface mb_linkable  src_modified =
                  compileOne' Nothing mHscMessage hsc_env summary' mod_index nmods
                             Nothing mb_linkable src_modified

            -- With NoBackend we create empty linkables to avoid recompilation.
            -- We have to detect these to recompile anyway if the backend changed
            -- since the last compile.
            is_fake_linkable
               | Just hmi <- old_hmi, Just l <- hm_linkable hmi =
                  null (linkableUnlinked l)
               | otherwise =
                   -- we have no linkable, so it cannot be fake
                   False

            implies False _ = True
            implies True x  = x

            debug_trace n t = liftIO $ debugTraceMsg (hsc_logger hsc_env) (hsc_dflags hsc_env) n t

        in
        case () of
         _
                -- Regardless of whether we're generating object code or
                -- byte code, we can always use an existing object file
                -- if it is *stable* (see checkStability).
          | is_stable_obj, Just hmi <- old_hmi -> do
                debug_trace 5 (text "skipping stable obj mod:" <+> ppr this_mod_name)
                return hmi
                -- object is stable, and we have an entry in the
                -- old HPT: nothing to do

          | is_stable_obj, isNothing old_hmi -> do
                debug_trace 5 (text "compiling stable on-disk mod:" <+> ppr this_mod_name)
                linkable <- liftIO $ findObjectLinkable this_mod obj_fn
                              (expectJust "upsweep1" mb_obj_date)
                compile_it (Just linkable) SourceUnmodifiedAndStable
                -- object is stable, but we need to load the interface
                -- off disk to make a HMI.

          | not (backendProducesObject bcknd), is_stable_bco,
            (bcknd /= NoBackend) `implies` not is_fake_linkable ->
                ASSERT(isJust old_hmi) -- must be in the old_hpt
                let Just hmi = old_hmi in do
                debug_trace 5 (text "skipping stable BCO mod:" <+> ppr this_mod_name)
                return hmi
                -- BCO is stable: nothing to do

          | not (backendProducesObject bcknd),
            Just hmi <- old_hmi,
            Just l <- hm_linkable hmi,
            not (isObjectLinkable l),
            (bcknd /= NoBackend) `implies` not is_fake_linkable,
            linkableTime l >= ms_hs_date summary -> do
                debug_trace 5 (text "compiling non-stable BCO mod:" <+> ppr this_mod_name)
                compile_it (Just l) SourceUnmodified
                -- we have an old BCO that is up to date with respect
                -- to the source: do a recompilation check as normal.

          -- When generating object code, if there's an up-to-date
          -- object file on the disk, then we can use it.
          -- However, if the object file is new (compared to any
          -- linkable we had from a previous compilation), then we
          -- must discard any in-memory interface, because this
          -- means the user has compiled the source file
          -- separately and generated a new interface, that we must
          -- read from the disk.
          --
          | backendProducesObject bcknd,
            Just obj_date <- mb_obj_date,
            obj_date >= hs_date -> do
                case old_hmi of
                  Just hmi
                    | Just l <- hm_linkable hmi,
                      isObjectLinkable l && linkableTime l == obj_date -> do
                          debug_trace 5 (text "compiling mod with new on-disk obj:" <+> ppr this_mod_name)
                          compile_it (Just l) SourceUnmodified
                  _otherwise -> do
                          debug_trace 5 (text "compiling mod with new on-disk obj2:" <+> ppr this_mod_name)
                          linkable <- liftIO $ findObjectLinkable this_mod obj_fn obj_date
                          compile_it_discard_iface (Just linkable) SourceUnmodified

          -- See Note [Recompilation checking in -fno-code mode]
          | writeInterfaceOnlyMode lcl_dflags,
            Just if_date <- mb_if_date,
            if_date >= hs_date -> do
                debug_trace 5 (text "skipping tc'd mod:" <+> ppr this_mod_name)
                compile_it Nothing SourceUnmodified

         _otherwise -> do
                debug_trace 5 (text "compiling mod:" <+> ppr this_mod_name)
                compile_it Nothing SourceModified


{- Note [-fno-code mode]
~~~~~~~~~~~~~~~~~~~~~~~~
GHC offers the flag -fno-code for the purpose of parsing and typechecking a
program without generating object files. This is intended to be used by tooling
and IDEs to provide quick feedback on any parser or type errors as cheaply as
possible.

When GHC is invoked with -fno-code no object files or linked output will be
generated. As many errors and warnings as possible will be generated, as if
-fno-code had not been passed. The session DynFlags will have
backend == NoBackend.

-fwrite-interface
~~~~~~~~~~~~~~~~
Whether interface files are generated in -fno-code mode is controlled by the
-fwrite-interface flag. The -fwrite-interface flag is a no-op if -fno-code is
not also passed. Recompilation avoidance requires interface files, so passing
-fno-code without -fwrite-interface should be avoided. If -fno-code were
re-implemented today, -fwrite-interface would be discarded and it would be
considered always on; this behaviour is as it is for backwards compatibility.

================================================================
IN SUMMARY: ALWAYS PASS -fno-code AND -fwrite-interface TOGETHER
================================================================

Template Haskell
~~~~~~~~~~~~~~~~
A module using template haskell may invoke an imported function from inside a
splice. This will cause the type-checker to attempt to execute that code, which
would fail if no object files had been generated. See #8025. To rectify this,
during the downsweep we patch the DynFlags in the ModSummary of any home module
that is imported by a module that uses template haskell, to generate object
code.

The flavour of generated object code is chosen by defaultObjectTarget for the
target platform. It would likely be faster to generate bytecode, but this is not
supported on all platforms(?Please Confirm?), and does not support the entirety
of GHC haskell. See #1257.

The object files (and interface files if -fwrite-interface is disabled) produced
for template haskell are written to temporary files.

Note that since template haskell can run arbitrary IO actions, -fno-code mode
is no more secure than running without it.

Potential TODOS:
~~~~~
* Remove -fwrite-interface and have interface files always written in -fno-code
  mode
* Both .o and .dyn_o files are generated for template haskell, but we only need
  .dyn_o. Fix it.
* In make mode, a message like
  Compiling A (A.hs, /tmp/ghc_123.o)
  is shown if downsweep enabled object code generation for A. Perhaps we should
  show "nothing" or "temporary object file" instead. Note that one
  can currently use -keep-tmp-files and inspect the generated file with the
  current behaviour.
* Offer a -no-codedir command line option, and write what were temporary
  object files there. This would speed up recompilation.
* Use existing object files (if they are up to date) instead of always
  generating temporary ones.
-}

-- Note [Recompilation checking in -fno-code mode]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- If we are compiling with -fno-code -fwrite-interface, there won't
-- be any object code that we can compare against, nor should there
-- be: we're *just* generating interface files.  In this case, we
-- want to check if the interface file is new, in lieu of the object
-- file.  See also #9243.

-- Filter modules in the HPT
retainInTopLevelEnvs :: [ModuleName] -> HomePackageTable -> HomePackageTable
retainInTopLevelEnvs keep_these hpt
   = listToHpt   [ (mod, expectJust "retain" mb_mod_info)
                 | mod <- keep_these
                 , let mb_mod_info = lookupHpt hpt mod
                 , isJust mb_mod_info ]

-- ---------------------------------------------------------------------------
-- Typecheck module loops
{-
See bug #930.  This code fixes a long-standing bug in --make.  The
problem is that when compiling the modules *inside* a loop, a data
type that is only defined at the top of the loop looks opaque; but
after the loop is done, the structure of the data type becomes
apparent.

The difficulty is then that two different bits of code have
different notions of what the data type looks like.

The idea is that after we compile a module which also has an .hs-boot
file, we re-generate the ModDetails for each of the modules that
depends on the .hs-boot file, so that everyone points to the proper
TyCons, Ids etc. defined by the real module, not the boot module.
Fortunately re-generating a ModDetails from a ModIface is easy: the
function GHC.IfaceToCore.typecheckIface does exactly that.

Picking the modules to re-typecheck is slightly tricky.  Starting from
the module graph consisting of the modules that have already been
compiled, we reverse the edges (so they point from the imported module
to the importing module), and depth-first-search from the .hs-boot
node.  This gives us all the modules that depend transitively on the
.hs-boot module, and those are exactly the modules that we need to
re-typecheck.

Following this fix, GHC can compile itself with --make -O2.
-}

reTypecheckLoop :: HscEnv -> ModSummary -> ModuleGraph -> IO HscEnv
reTypecheckLoop hsc_env ms graph
  | Just loop <- getModLoop ms mss appearsAsBoot
  -- SOME hs-boot files should still
  -- get used, just not the loop-closer.
  , let non_boot = flip mapMaybe loop $ \case
          InstantiationNode _ -> Nothing
          ModuleNode ems -> do
            let l = emsModSummary ems
            guard $ not $ isBootSummary l == IsBoot && ms_mod l == ms_mod ms
            pure l
  = typecheckLoop (hsc_dflags hsc_env) hsc_env (map ms_mod_name non_boot)
  | otherwise
  = return hsc_env
  where
  mss = mgModSummaries' graph
  appearsAsBoot = (`elemModuleSet` mgBootModules graph)

-- | Given a non-boot ModSummary @ms@ of a module, for which there exists a
-- corresponding boot file in @graph@, return the set of modules which
-- transitively depend on this boot file.  This function is slightly misnamed,
-- but its name "getModLoop" alludes to the fact that, when getModLoop is called
-- with a graph that does not contain @ms@ (non-parallel case) or is an
-- SCC with hs-boot nodes dropped (parallel-case), the modules which
-- depend on the hs-boot file are typically (but not always) the
-- modules participating in the recursive module loop.  The returned
-- list includes the hs-boot file.
--
-- Example:
--      let g represent the module graph:
--          C.hs
--          A.hs-boot imports C.hs
--          B.hs imports A.hs-boot
--          A.hs imports B.hs
--      genModLoop A.hs g == Just [A.hs-boot, B.hs, A.hs]
--
--      It would also be permissible to omit A.hs from the graph,
--      in which case the result is [A.hs-boot, B.hs]
--
-- Example:
--      A counter-example to the claim that modules returned
--      by this function participate in the loop occurs here:
--
--      let g represent the module graph:
--          C.hs
--          A.hs-boot imports C.hs
--          B.hs imports A.hs-boot
--          A.hs imports B.hs
--          D.hs imports A.hs-boot
--      genModLoop A.hs g == Just [A.hs-boot, B.hs, A.hs, D.hs]
--
--      Arguably, D.hs should import A.hs, not A.hs-boot, but
--      a dependency on the boot file is not illegal.
--
getModLoop
  :: ModSummary
  -> [ModuleGraphNode]
  -> (Module -> Bool) -- check if a module appears as a boot module in 'graph'
  -> Maybe [ModuleGraphNode]
getModLoop ms graph appearsAsBoot
  | isBootSummary ms == NotBoot
  , appearsAsBoot this_mod
  , let mss = reachableBackwards (ms_mod_name ms) graph
  = Just mss
  | otherwise
  = Nothing
 where
  this_mod = ms_mod ms

-- NB: sometimes mods has duplicates; this is harmless because
-- any duplicates get clobbered in addListToHpt and never get forced.
typecheckLoop :: DynFlags -> HscEnv -> [ModuleName] -> IO HscEnv
typecheckLoop dflags hsc_env mods = do
  debugTraceMsg logger dflags 2 $
     text "Re-typechecking loop: " GHC.Utils.Outputable.<> ppr mods
  new_hpt <-
    fixIO $ \new_hpt -> do
      let new_hsc_env = hsc_env{ hsc_HPT = new_hpt }
      mds <- initIfaceCheck (text "typecheckLoop") new_hsc_env $
                mapM (typecheckIface . hm_iface) hmis
      let new_hpt = addListToHpt old_hpt
                        (zip mods [ hmi{ hm_details = details }
                                  | (hmi,details) <- zip hmis mds ])
      return new_hpt
  return hsc_env{ hsc_HPT = new_hpt }
  where
    logger  = hsc_logger hsc_env
    old_hpt = hsc_HPT hsc_env
    hmis    = map (expectJust "typecheckLoop" . lookupHpt old_hpt) mods

reachableBackwards :: ModuleName -> [ModuleGraphNode] -> [ModuleGraphNode]
reachableBackwards mod summaries
  = [ node_payload node | node <- reachableG (transposeG graph) root ]
  where -- the rest just sets up the graph:
        (graph, lookup_node) = moduleGraphNodes False summaries
        root  = expectJust "reachableBackwards" (lookup_node $ NodeKey_Module $ GWIB mod IsBoot)

-- ---------------------------------------------------------------------------
--
-- | Topological sort of the module graph
topSortModuleGraph
          :: Bool
          -- ^ Drop hi-boot nodes? (see below)
          -> ModuleGraph
          -> Maybe ModuleName
             -- ^ Root module name.  If @Nothing@, use the full graph.
          -> [SCC ModuleGraphNode]
-- ^ Calculate SCCs of the module graph, possibly dropping the hi-boot nodes
-- The resulting list of strongly-connected-components is in topologically
-- sorted order, starting with the module(s) at the bottom of the
-- dependency graph (ie compile them first) and ending with the ones at
-- the top.
--
-- Drop hi-boot nodes (first boolean arg)?
--
-- - @False@:   treat the hi-boot summaries as nodes of the graph,
--              so the graph must be acyclic
--
-- - @True@:    eliminate the hi-boot nodes, and instead pretend
--              the a source-import of Foo is an import of Foo
--              The resulting graph has no hi-boot nodes, but can be cyclic

topSortModuleGraph drop_hs_boot_nodes module_graph mb_root_mod
  = map (fmap summaryNodeSummary) $ stronglyConnCompG initial_graph
  where
    summaries = mgModSummaries' module_graph
    -- stronglyConnCompG flips the original order, so if we reverse
    -- the summaries we get a stable topological sort.
    (graph, lookup_node) =
      moduleGraphNodes drop_hs_boot_nodes (reverse summaries)

    initial_graph = case mb_root_mod of
        Nothing -> graph
        Just root_mod ->
            -- restrict the graph to just those modules reachable from
            -- the specified module.  We do this by building a graph with
            -- the full set of nodes, and determining the reachable set from
            -- the specified node.
            let root | Just node <- lookup_node $ NodeKey_Module $ GWIB root_mod NotBoot
                     , graph `hasVertexG` node
                     = node
                     | otherwise
                     = throwGhcException (ProgramError "module does not exist")
            in graphFromEdgedVerticesUniq (seq root (reachableG graph root))

type SummaryNode = Node Int ModuleGraphNode

summaryNodeKey :: SummaryNode -> Int
summaryNodeKey = node_key

summaryNodeSummary :: SummaryNode -> ModuleGraphNode
summaryNodeSummary = node_payload

-- | Collect the immediate dependencies of a ModuleGraphNode,
-- optionally avoiding hs-boot dependencies.
-- If the drop_hs_boot_nodes flag is False, and if this is a .hs and there is
-- an equivalent .hs-boot, add a link from the former to the latter.  This
-- has the effect of detecting bogus cases where the .hs-boot depends on the
-- .hs, by introducing a cycle.  Additionally, it ensures that we will always
-- process the .hs-boot before the .hs, and so the HomePackageTable will always
-- have the most up to date information.
unfilteredEdges :: Bool -> ModuleGraphNode -> [NodeKey]
unfilteredEdges drop_hs_boot_nodes = \case
    InstantiationNode iuid ->
      NodeKey_Module . flip GWIB NotBoot <$> uniqDSetToList (instUnitHoles iuid)
    ModuleNode (ExtendedModSummary ms bds) ->
      (NodeKey_Module . flip GWIB hs_boot_key . unLoc <$> ms_home_srcimps ms) ++
      (NodeKey_Module . flip GWIB NotBoot     . unLoc <$> ms_home_imps ms) ++
      [ NodeKey_Module $ GWIB (ms_mod_name ms) IsBoot
      | not $ drop_hs_boot_nodes || ms_hsc_src ms == HsBootFile
      ] ++
      [ NodeKey_Unit inst_unit
      | inst_unit <- bds
      ]
  where
    -- Drop hs-boot nodes by using HsSrcFile as the key
    hs_boot_key | drop_hs_boot_nodes = NotBoot -- is regular mod or signature
                | otherwise          = IsBoot

moduleGraphNodes :: Bool -> [ModuleGraphNode]
  -> (Graph SummaryNode, NodeKey -> Maybe SummaryNode)
moduleGraphNodes drop_hs_boot_nodes summaries =
  (graphFromEdgedVerticesUniq nodes, lookup_node)
  where
    numbered_summaries = zip summaries [1..]

    lookup_node :: NodeKey -> Maybe SummaryNode
    lookup_node key = Map.lookup key (unNodeMap node_map)

    lookup_key :: NodeKey -> Maybe Int
    lookup_key = fmap summaryNodeKey . lookup_node

    node_map :: NodeMap SummaryNode
    node_map = NodeMap $
      Map.fromList [ (mkHomeBuildModule s, node)
                   | node <- nodes
                   , let s = summaryNodeSummary node
                   ]

    -- We use integers as the keys for the SCC algorithm
    nodes :: [SummaryNode]
    nodes = [ DigraphNode s key $ out_edge_keys $ unfilteredEdges drop_hs_boot_nodes s
            | (s, key) <- numbered_summaries
             -- Drop the hi-boot ones if told to do so
            , case s of
                InstantiationNode _ -> True
                ModuleNode ems -> not $ isBootSummary (emsModSummary ems) == IsBoot && drop_hs_boot_nodes
            ]

    out_edge_keys :: [NodeKey] -> [Int]
    out_edge_keys = mapMaybe lookup_key
        -- If we want keep_hi_boot_nodes, then we do lookup_key with
        -- IsBoot; else False

-- The nodes of the graph are keyed by (mod, is boot?) pairs for the current
-- modules, and indefinite unit IDs for dependencies which are instantiated with
-- our holes.
--
-- NB: hsig files show up as *normal* nodes (not boot!), since they don't
-- participate in cycles (for now)
type ModNodeKey = ModuleNameWithIsBoot
newtype ModNodeMap a = ModNodeMap { unModNodeMap :: Map.Map ModNodeKey a }
  deriving (Functor, Traversable, Foldable)

emptyModNodeMap :: ModNodeMap a
emptyModNodeMap = ModNodeMap Map.empty

modNodeMapInsert :: ModNodeKey -> a -> ModNodeMap a -> ModNodeMap a
modNodeMapInsert k v (ModNodeMap m) = ModNodeMap (Map.insert k v m)

modNodeMapElems :: ModNodeMap a -> [a]
modNodeMapElems (ModNodeMap m) = Map.elems m

modNodeMapLookup :: ModNodeKey -> ModNodeMap a -> Maybe a
modNodeMapLookup k (ModNodeMap m) = Map.lookup k m

data NodeKey = NodeKey_Unit {-# UNPACK #-} !InstantiatedUnit | NodeKey_Module {-# UNPACK #-} !ModNodeKey
  deriving (Eq, Ord)

newtype NodeMap a = NodeMap { unNodeMap :: Map.Map NodeKey a }
  deriving (Functor, Traversable, Foldable)

msKey :: ModSummary -> ModNodeKey
msKey = mkHomeBuildModule0

mkNodeKey :: ModuleGraphNode -> NodeKey
mkNodeKey = \case
  InstantiationNode x -> NodeKey_Unit x
  ModuleNode x -> NodeKey_Module $ mkHomeBuildModule0 (emsModSummary x)

pprNodeKey :: NodeKey -> SDoc
pprNodeKey (NodeKey_Unit iu) = ppr iu
pprNodeKey (NodeKey_Module mk) = ppr mk

mkNodeMap :: [ExtendedModSummary] -> ModNodeMap ExtendedModSummary
mkNodeMap summaries = ModNodeMap $ Map.fromList
  [ (msKey $ emsModSummary s, s) | s <- summaries]

-- | If there are {-# SOURCE #-} imports between strongly connected
-- components in the topological sort, then those imports can
-- definitely be replaced by ordinary non-SOURCE imports: if SOURCE
-- were necessary, then the edge would be part of a cycle.
warnUnnecessarySourceImports :: GhcMonad m => [SCC ModSummary] -> m ()
warnUnnecessarySourceImports sccs = do
  dflags <- getDynFlags
  when (wopt Opt_WarnUnusedImports dflags)
    (logWarnings (listToBag (concatMap (check . flattenSCC) sccs)))
  where check ms =
           let mods_in_this_cycle = map ms_mod_name ms in
           [ warn i | m <- ms, i <- ms_home_srcimps m,
                      unLoc i `notElem`  mods_in_this_cycle ]

        warn :: Located ModuleName -> WarnMsg
        warn (L loc mod) =
           mkPlainMsgEnvelope loc
                (text "Warning: {-# SOURCE #-} unnecessary in import of "
                 <+> quotes (ppr mod))


-----------------------------------------------------------------------------
--
-- | Downsweep (dependency analysis)
--
-- Chase downwards from the specified root set, returning summaries
-- for all home modules encountered.  Only follow source-import
-- links.
--
-- We pass in the previous collection of summaries, which is used as a
-- cache to avoid recalculating a module summary if the source is
-- unchanged.
--
-- The returned list of [ModSummary] nodes has one node for each home-package
-- module, plus one for any hs-boot files.  The imports of these nodes
-- are all there, including the imports of non-home-package modules.
-- downsweep :: HscEnv
--           -> [ExtendedModSummary]
--           -- ^ Old summaries
--           -> [ModuleName]       -- Ignore dependencies on these; treat
--                                 -- them as if they were package modules
--           -> Bool               -- True <=> allow multiple targets to have
--                                 --          the same module name; this is
--                                 --          very useful for ghc -M
--           -> IO [Either ErrorMessages ExtendedModSummary]
--                 -- The non-error elements of the returned list all have distinct
--                 -- (Modules, IsBoot) identifiers, unless the Bool is true in
--                 -- which case there can be repeats
-- downsweep hsc_env old_summaries excl_mods allow_dup_roots
--    = do
--        rootSummaries <- mapM getRootSummary roots
--        let (errs, rootSummariesOk) = partitionEithers rootSummaries -- #17549
--            root_map = mkRootMap rootSummariesOk
--        checkDuplicates root_map
--        map0 <- loop (concatMap calcDeps rootSummariesOk) root_map
--        -- if we have been passed -fno-code, we enable code generation
--        -- for dependencies of modules that have -XTemplateHaskell,
--        -- otherwise those modules will fail to compile.
--        -- See Note [-fno-code mode] #8025
--        let default_backend = platformDefaultBackend (targetPlatform dflags)
--        let home_unit       = hsc_home_unit hsc_env
--        let tmpfs           = hsc_tmpfs     hsc_env
--        map1 <- case backend dflags of
--          NoBackend   -> enableCodeGenForTH logger tmpfs home_unit default_backend map0
--          _           -> return map0
--        if null errs
--          then pure $ concat $ modNodeMapElems map1
--          else pure $ map Left errs
--      where
--         -- TODO(@Ericson2314): Probably want to include backpack instantiations
--         -- in the map eventually for uniformity
--         calcDeps (ExtendedModSummary ms _bkp_deps) = msDeps ms

--         dflags = hsc_dflags hsc_env
--         logger = hsc_logger hsc_env
--         roots = hsc_targets hsc_env

--         old_summary_map :: ModNodeMap ExtendedModSummary
--         old_summary_map = mkNodeMap old_summaries

--         getRootSummary :: Target -> IO (Either ErrorMessages ExtendedModSummary)
--         getRootSummary (Target (TargetFile file mb_phase) obj_allowed maybe_buf)
--            = do exists <- liftIO $ doesFileExist file
--                 if exists || isJust maybe_buf
--                     then summariseFile hsc_env old_summaries file mb_phase
--                                        obj_allowed maybe_buf
--                     else return $ Left $ unitBag $ mkPlainMsgEnvelope noSrcSpan $
--                            text "can't find file:" <+> text file
--         getRootSummary (Target (TargetModule modl) obj_allowed maybe_buf)
--            = do maybe_summary <- summariseModule hsc_env old_summary_map NotBoot
--                                            (L rootLoc modl) obj_allowed
--                                            maybe_buf excl_mods
--                 case maybe_summary of
--                    Nothing -> return $ Left $ moduleNotFoundErr modl
--                    Just s  -> return s

--         rootLoc = mkGeneralSrcSpan (fsLit "<command line>")

--         -- In a root module, the filename is allowed to diverge from the module
--         -- name, so we have to check that there aren't multiple root files
--         -- defining the same module (otherwise the duplicates will be silently
--         -- ignored, leading to confusing behaviour).
--         checkDuplicates
--           :: ModNodeMap
--                [Either ErrorMessages
--                        ExtendedModSummary]
--           -> IO ()
--         checkDuplicates root_map
--            | allow_dup_roots = return ()
--            | null dup_roots  = return ()
--            | otherwise       = liftIO $ multiRootsErr (emsModSummary <$> head dup_roots)
--            where
--              dup_roots :: [[ExtendedModSummary]]        -- Each at least of length 2
--              dup_roots = filterOut isSingleton $ map rights $ modNodeMapElems root_map

--         loop :: [GenWithIsBoot (Located ModuleName)]
--                         -- Work list: process these modules
--              -> ModNodeMap [Either ErrorMessages ExtendedModSummary]
--                         -- Visited set; the range is a list because
--                         -- the roots can have the same module names
--                         -- if allow_dup_roots is True
--              -> IO (ModNodeMap [Either ErrorMessages ExtendedModSummary])
--                         -- The result is the completed NodeMap
--         loop [] done = return done
--         loop (s : ss) done
--           | Just summs <- modNodeMapLookup key done
--           = if isSingleton summs then
--                 loop ss done
--             else
--                 do { multiRootsErr (emsModSummary <$> rights summs)
--                    ; return (ModNodeMap Map.empty)
--                    }
--           | otherwise
--           = do mb_s <- summariseModule hsc_env old_summary_map
--                                        is_boot wanted_mod True
--                                        Nothing excl_mods
--                case mb_s of
--                    Nothing -> loop ss done
--                    Just (Left e) -> loop ss (modNodeMapInsert key [Left e] done)
--                    Just (Right s)-> do
--                      new_map <-
--                        loop (calcDeps s) (modNodeMapInsert key [Right s] done)
--                      loop ss new_map
--           where
--             GWIB { gwib_mod = L loc mod, gwib_isBoot = is_boot } = s
--             wanted_mod = L loc mod
--             key = GWIB
--                     { gwib_mod = unLoc wanted_mod
--                     , gwib_isBoot = is_boot
--                     }

-- | Update the every ModSummary that is depended on
-- by a module that needs template haskell. We enable codegen to
-- the specified target, disable optimization and change the .hi
-- and .o file locations to be temporary files.
-- See Note [-fno-code mode]
enableCodeGenForTH
  :: Logger
  -> TmpFs
  -> HomeUnit
  -> Backend
  -> ModNodeMap [Either ErrorMessages ExtendedModSummary]
  -> IO (ModNodeMap [Either ErrorMessages ExtendedModSummary])
enableCodeGenForTH logger tmpfs home_unit =
  enableCodeGenWhen logger tmpfs condition should_modify TFL_CurrentModule TFL_GhcSession
  where
    condition = isTemplateHaskellOrQQNonBoot
    should_modify (ModSummary { ms_hspp_opts = dflags }) =
      backend dflags == NoBackend &&
      -- Don't enable codegen for TH on indefinite packages; we
      -- can't compile anything anyway! See #16219.
      isHomeUnitDefinite home_unit

-- | Helper used to implement 'enableCodeGenForTH'.
-- In particular, this enables
-- unoptimized code generation for all modules that meet some
-- condition (first parameter), or are dependencies of those
-- modules. The second parameter is a condition to check before
-- marking modules for code generation.
enableCodeGenWhen
  :: Logger
  -> TmpFs
  -> (ModSummary -> Bool)
  -> (ModSummary -> Bool)
  -> TempFileLifetime
  -> TempFileLifetime
  -> Backend
  -> ModNodeMap [Either ErrorMessages ExtendedModSummary]
  -> IO (ModNodeMap [Either ErrorMessages ExtendedModSummary])
enableCodeGenWhen logger tmpfs condition should_modify staticLife dynLife bcknd nodemap =
  traverse (traverse (traverse enable_code_gen)) nodemap
  where
    enable_code_gen :: ExtendedModSummary -> IO ExtendedModSummary
    enable_code_gen (ExtendedModSummary ms bkp_deps)
      | ModSummary
        { ms_mod = ms_mod
        , ms_location = ms_location
        , ms_hsc_src = HsSrcFile
        , ms_hspp_opts = dflags
        } <- ms
      , should_modify ms
      , ms_mod `Set.member` needs_codegen_set
      = do
        let new_temp_file suf dynsuf = do
              tn <- newTempName logger tmpfs dflags staticLife suf
              let dyn_tn = tn -<.> dynsuf
              addFilesToClean tmpfs dynLife [dyn_tn]
              return tn
          -- We don't want to create .o or .hi files unless we have been asked
          -- to by the user. But we need them, so we patch their locations in
          -- the ModSummary with temporary files.
          --
        (hi_file, o_file) <-
          -- If ``-fwrite-interface` is specified, then the .o and .hi files
          -- are written into `-odir` and `-hidir` respectively.  #16670
          if gopt Opt_WriteInterface dflags
            then return (ml_hi_file ms_location, ml_obj_file ms_location)
            else (,) <$> (new_temp_file (hiSuf_ dflags) (dynHiSuf_ dflags))
                     <*> (new_temp_file (objectSuf_ dflags) (dynObjectSuf_ dflags))
        let ms' = ms
              { ms_location =
                  ms_location {ml_hi_file = hi_file, ml_obj_file = o_file}
              , ms_hspp_opts = updOptLevel 0 $ dflags {backend = bcknd}
              }
        pure (ExtendedModSummary ms' bkp_deps)
      | otherwise = return (ExtendedModSummary ms bkp_deps)

    needs_codegen_set = transitive_deps_set
      [ ms
      | mss <- modNodeMapElems nodemap
      , Right (ExtendedModSummary { emsModSummary = ms }) <- mss
      , condition ms
      ]

    -- find the set of all transitive dependencies of a list of modules.
    transitive_deps_set :: [ModSummary] -> Set.Set Module
    transitive_deps_set modSums = foldl' go Set.empty modSums
      where
        go marked_mods ms@ModSummary{ms_mod}
          | ms_mod `Set.member` marked_mods = marked_mods
          | otherwise =
            let deps =
                  [ dep_ms
                  -- If a module imports a boot module, msDeps helpfully adds a
                  -- dependency to that non-boot module in it's result. This
                  -- means we don't have to think about boot modules here.
                  | dep <- msDeps ms
                  , NotBoot == gwib_isBoot dep
                  , dep_ms_0 <- toList $ modNodeMapLookup (unLoc <$> dep) nodemap
                  , dep_ms_1 <- toList $ dep_ms_0
                  , (ExtendedModSummary { emsModSummary = dep_ms }) <- toList $ dep_ms_1
                  ]
                new_marked_mods = Set.insert ms_mod marked_mods
            in foldl' go new_marked_mods deps

mkRootMap
  :: [ExtendedModSummary]
  -> ModNodeMap [Either ErrorMessages ExtendedModSummary]
mkRootMap summaries = ModNodeMap $ Map.insertListWith
  (flip (++))
  [ (msKey $ emsModSummary s, [Right s]) | s <- summaries ]
  Map.empty

-- | Returns the dependencies of the ModSummary s.
-- A wrinkle is that for a {-# SOURCE #-} import we return
--      *both* the hs-boot file
--      *and* the source file
-- as "dependencies".  That ensures that the list of all relevant
-- modules always contains B.hs if it contains B.hs-boot.
-- Remember, this pass isn't doing the topological sort.  It's
-- just gathering the list of all relevant ModSummaries
msDeps :: ModSummary -> [GenWithIsBoot (Located ModuleName)]
msDeps s = [ d
           | m <- ms_home_srcimps s
           , d <- [ GWIB { gwib_mod = m, gwib_isBoot = IsBoot }
                  , GWIB { gwib_mod = m, gwib_isBoot = NotBoot }
                  ]
           ]
        ++ [ GWIB { gwib_mod = m, gwib_isBoot = NotBoot }
           | m <- ms_home_imps s
           ]

-----------------------------------------------------------------------------
-- Summarising modules

-- We have two types of summarisation:
--
--    * Summarise a file.  This is used for the root module(s) passed to
--      cmLoadModules.  The file is read, and used to determine the root
--      module name.  The module name may differ from the filename.
--
--    * Summarise a module.  We are given a module name, and must provide
--      a summary.  The finder is used to locate the file in which the module
--      resides.

-- summariseFile
--         :: HscEnv
--         -> [ExtendedModSummary]         -- old summaries
--         -> FilePath                     -- source file name
--         -> Maybe Phase                  -- start phase
--         -> Bool                         -- object code allowed?
--         -> Maybe (StringBuffer,UTCTime)
--         -> IO (Either ErrorMessages ExtendedModSummary)

-- summariseFile hsc_env old_summaries src_fn mb_phase obj_allowed maybe_buf
--         -- we can use a cached summary if one is available and the
--         -- source file hasn't changed,  But we have to look up the summary
--         -- by source file, rather than module name as we do in summarise.
--    | Just old_summary <- findSummaryBySourceFile old_summaries src_fn
--    = do
--         let location = ms_location $ emsModSummary old_summary
--             dflags = hsc_dflags hsc_env

--         src_timestamp <- get_src_timestamp
--                 -- The file exists; we checked in getRootSummary above.
--                 -- If it gets removed subsequently, then this
--                 -- getModificationUTCTime may fail, but that's the right
--                 -- behaviour.

--                 -- return the cached summary if the source didn't change
--         checkSummaryTimestamp
--             hsc_env dflags obj_allowed NotBoot (new_summary src_fn)
--             old_summary location src_timestamp

--    | otherwise
--    = do src_timestamp <- get_src_timestamp
--         new_summary src_fn src_timestamp
--   where
--     get_src_timestamp = case maybe_buf of
--                            Just (_,t) -> return t
--                            Nothing    -> liftIO $ getModificationUTCTime src_fn
--                         -- getModificationUTCTime may fail

--     new_summary src_fn src_timestamp = runExceptT $ do
--         preimps@PreprocessedImports {..}
--             <- getPreprocessedImports hsc_env src_fn mb_phase maybe_buf


--         -- Make a ModLocation for this file
--         location <- liftIO $ mkHomeModLocation (hsc_dflags hsc_env) pi_mod_name src_fn

--         -- Tell the Finder cache where it is, so that subsequent calls
--         -- to findModule will find it, even if it's not on any search path
--         mod <- liftIO $ addHomeModuleToFinder hsc_env pi_mod_name location

--         liftIO $ makeNewModSummary hsc_env $ MakeNewModSummary
--             { nms_src_fn = src_fn
--             , nms_src_timestamp = src_timestamp
--             , nms_is_boot = NotBoot
--             , nms_hsc_src =
--                 if isHaskellSigFilename src_fn
--                    then HsigFile
--                    else HsSrcFile
--             , nms_location = location
--             , nms_mod = mod
--             , nms_obj_allowed = obj_allowed
--             , nms_preimps = preimps
--             }

findSummaryBySourceFile :: [ExtendedModSummary] -> FilePath -> Maybe ExtendedModSummary
findSummaryBySourceFile summaries file = case
    [ ms
    | ms <- summaries
    , HsSrcFile <- [ms_hsc_src $ emsModSummary ms]
    , let derived_file = ml_hs_file $ ms_location $ emsModSummary ms
    , expectJust "findSummaryBySourceFile" derived_file == file
    ]
  of
    [] -> Nothing
    (x:_) -> Just x

-- checkSummaryTimestamp
--     :: HscEnv -> DynFlags -> Bool -> IsBootInterface
--     -> (UTCTime -> IO (Either e ExtendedModSummary))
--     -> ExtendedModSummary -> ModLocation -> UTCTime
--     -> IO (Either e ExtendedModSummary)
-- checkSummaryTimestamp
--   hsc_env dflags obj_allowed is_boot new_summary
--   (ExtendedModSummary { emsModSummary = old_summary, emsInstantiatedUnits = bkp_deps})
--   location src_timestamp
--   | ms_hs_date old_summary == src_timestamp &&
--       not (gopt Opt_ForceRecomp (hsc_dflags hsc_env)) = do
--            -- update the object-file timestamp
--            obj_timestamp <-
--              if backendProducesObject (backend (hsc_dflags hsc_env))
--                  || obj_allowed -- bug #1205
--                  then liftIO $ getObjTimestamp location is_boot
--                  else return Nothing

--            -- We have to repopulate the Finder's cache for file targets
--            -- because the file might not even be on the regular search path
--            -- and it was likely flushed in depanal. This is not technically
--            -- needed when we're called from sumariseModule but it shouldn't
--            -- hurt.
--            _ <- addHomeModuleToFinder hsc_env
--                   (moduleName (ms_mod old_summary)) location

--            hi_timestamp <- maybeGetIfaceDate dflags location
--            hie_timestamp <- modificationTimeIfExists (ml_hie_file location)

--            return $ Right
--              ( ExtendedModSummary { emsModSummary = old_summary
--                      { ms_obj_date = obj_timestamp
--                      , ms_iface_date = hi_timestamp
--                      , ms_hie_date = hie_timestamp
--                      }
--                    , emsInstantiatedUnits = bkp_deps
--                    }
--              )

--    | otherwise =
--            -- source changed: re-summarise.
--            new_summary src_timestamp

-- Summarise a module, and pick up source and timestamp.
-- summariseModule
--           :: HscEnv
--           -> ModNodeMap ExtendedModSummary
--           -- ^ Map of old summaries
--           -> IsBootInterface    -- True <=> a {-# SOURCE #-} import
--           -> Located ModuleName -- Imported module to be summarised
--           -> Bool               -- object code allowed?
--           -> Maybe (StringBuffer, UTCTime)
--           -> [ModuleName]               -- Modules to exclude
--           -> IO (Maybe (Either ErrorMessages ExtendedModSummary))      -- Its new summary

-- summariseModule hsc_env old_summary_map is_boot (L loc wanted_mod)
--                 obj_allowed maybe_buf excl_mods
--   | wanted_mod `elem` excl_mods
--   = return Nothing

--   | Just old_summary <- modNodeMapLookup
--       (GWIB { gwib_mod = wanted_mod, gwib_isBoot = is_boot })
--       old_summary_map
--   = do          -- Find its new timestamp; all the
--                 -- ModSummaries in the old map have valid ml_hs_files
--         let location = ms_location $ emsModSummary old_summary
--             src_fn = expectJust "summariseModule" (ml_hs_file location)

--                 -- check the modification time on the source file, and
--                 -- return the cached summary if it hasn't changed.  If the
--                 -- file has disappeared, we need to call the Finder again.
--         case maybe_buf of
--            Just (_,t) ->
--                Just <$> check_timestamp old_summary location src_fn t
--            Nothing    -> do
--                 m <- tryIO (getModificationUTCTime src_fn)
--                 case m of
--                    Right t ->
--                        Just <$> check_timestamp old_summary location src_fn t
--                    Left e | isDoesNotExistError e -> find_it
--                           | otherwise             -> ioError e

--   | otherwise  = find_it
--   where
--     dflags = hsc_dflags hsc_env
--     home_unit = hsc_home_unit hsc_env

--     check_timestamp old_summary location src_fn =
--         checkSummaryTimestamp
--           hsc_env dflags obj_allowed is_boot
--           (new_summary location (ms_mod $ emsModSummary old_summary) src_fn)
--           old_summary location

--     find_it = do
--         found <- findImportedModule hsc_env wanted_mod Nothing
--         case found of
--              Found location mod
--                 | isJust (ml_hs_file location) ->
--                         -- Home package
--                          Just <$> just_found location mod

--              _ -> return Nothing
--                         -- Not found
--                         -- (If it is TRULY not found at all, we'll
--                         -- error when we actually try to compile)

--     just_found location mod = do
--                 -- Adjust location to point to the hs-boot source file,
--                 -- hi file, object file, when is_boot says so
--         let location' = case is_boot of
--               IsBoot -> addBootSuffixLocn location
--               NotBoot -> location
--             src_fn = expectJust "summarise2" (ml_hs_file location')

--                 -- Check that it exists
--                 -- It might have been deleted since the Finder last found it
--         maybe_t <- modificationTimeIfExists src_fn
--         case maybe_t of
--           Nothing -> return $ Left $ noHsFileErr loc src_fn
--           Just t  -> new_summary location' mod src_fn t

--     new_summary location mod src_fn src_timestamp
--       = runExceptT $ do
--         preimps@PreprocessedImports {..}
--             <- getPreprocessedImports hsc_env src_fn Nothing maybe_buf

--         -- NB: Despite the fact that is_boot is a top-level parameter, we
--         -- don't actually know coming into this function what the HscSource
--         -- of the module in question is.  This is because we may be processing
--         -- this module because another module in the graph imported it: in this
--         -- case, we know if it's a boot or not because of the {-# SOURCE #-}
--         -- annotation, but we don't know if it's a signature or a regular
--         -- module until we actually look it up on the filesystem.
--         let hsc_src
--               | is_boot == IsBoot = HsBootFile
--               | isHaskellSigFilename src_fn = HsigFile
--               | otherwise = HsSrcFile

--         when (pi_mod_name /= wanted_mod) $
--                 throwE $ unitBag $ mkPlainMsgEnvelope pi_mod_name_loc $
--                               text "File name does not match module name:"
--                               $$ text "Saw:" <+> quotes (ppr pi_mod_name)
--                               $$ text "Expected:" <+> quotes (ppr wanted_mod)

--         when (hsc_src == HsigFile && isNothing (lookup pi_mod_name (homeUnitInstantiations home_unit))) $
--             let suggested_instantiated_with =
--                     hcat (punctuate comma $
--                         [ ppr k <> text "=" <> ppr v
--                         | (k,v) <- ((pi_mod_name, mkHoleModule pi_mod_name)
--                                 : homeUnitInstantiations home_unit)
--                         ])
--             in throwE $ unitBag $ mkPlainMsgEnvelope pi_mod_name_loc $
--                 text "Unexpected signature:" <+> quotes (ppr pi_mod_name)
--                 $$ if gopt Opt_BuildingCabalPackage dflags
--                     then parens (text "Try adding" <+> quotes (ppr pi_mod_name)
--                             <+> text "to the"
--                             <+> quotes (text "signatures")
--                             <+> text "field in your Cabal file.")
--                     else parens (text "Try passing -instantiated-with=\"" <>
--                                  suggested_instantiated_with <> text "\"" $$
--                                 text "replacing <" <> ppr pi_mod_name <> text "> as necessary.")

--         liftIO $ makeNewModSummary hsc_env $ MakeNewModSummary
--             { nms_src_fn = src_fn
--             , nms_src_timestamp = src_timestamp
--             , nms_is_boot = is_boot
--             , nms_hsc_src = hsc_src
--             , nms_location = location
--             , nms_mod = mod
--             , nms_obj_allowed = obj_allowed
--             , nms_preimps = preimps
--             }

-- | Convenience named arguments for 'makeNewModSummary' only used to make
-- code more readable, not exported.
-- data MakeNewModSummary
--   = MakeNewModSummary
--       { nms_src_fn :: FilePath
--       , nms_src_timestamp :: UTCTime
--       , nms_is_boot :: IsBootInterface
--       , nms_hsc_src :: HscSource
--       , nms_location :: ModLocation
--       , nms_mod :: Module
--       , nms_obj_allowed :: Bool
--       , nms_preimps :: PreprocessedImports
--       }

-- makeNewModSummary :: HscEnv -> MakeNewModSummary -> IO ExtendedModSummary
-- makeNewModSummary hsc_env MakeNewModSummary{..} = do
--   let PreprocessedImports{..} = nms_preimps
--   let dflags = hsc_dflags hsc_env

--   -- when the user asks to load a source file by name, we only
--   -- use an object file if -fobject-code is on.  See #1205.
--   obj_timestamp <- liftIO $
--       if backendProducesObject (backend dflags)
--          || nms_obj_allowed -- bug #1205
--           then getObjTimestamp nms_location nms_is_boot
--           else return Nothing

--   hi_timestamp <- maybeGetIfaceDate dflags nms_location
--   hie_timestamp <- modificationTimeIfExists (ml_hie_file nms_location)

--   extra_sig_imports <- findExtraSigImports hsc_env nms_hsc_src pi_mod_name
--   (implicit_sigs, inst_deps) <- implicitRequirementsShallow hsc_env pi_theimps

--   return $ ExtendedModSummary
--     { emsModSummary =
--         ModSummary
--         { ms_mod = nms_mod
--         , ms_hsc_src = nms_hsc_src
--         , ms_location = nms_location
--         , ms_hspp_file = pi_hspp_fn
--         , ms_hspp_opts = pi_local_dflags
--         , ms_hspp_buf  = Just pi_hspp_buf
--         , ms_parsed_mod = Nothing
--         , ms_srcimps = pi_srcimps
--         , ms_textual_imps =
--             pi_theimps ++
--             extra_sig_imports ++
--             ((,) Nothing . noLoc <$> implicit_sigs)
--         , ms_hs_date = nms_src_timestamp
--         , ms_iface_date = hi_timestamp
--         , ms_hie_date = hie_timestamp
--         , ms_obj_date = obj_timestamp
--         }
--     , emsInstantiatedUnits = inst_deps
--     }

-- getObjTimestamp :: ModLocation -> IsBootInterface -> IO (Maybe UTCTime)
-- getObjTimestamp location is_boot
--   = case is_boot of
--       IsBoot -> return Nothing
--       NotBoot -> modificationTimeIfExists (ml_obj_file location)

-- data PreprocessedImports
--   = PreprocessedImports
--       { pi_local_dflags :: DynFlags
--       , pi_srcimps  :: [(Maybe FastString, Located ModuleName)]
--       , pi_theimps  :: [(Maybe FastString, Located ModuleName)]
--       , pi_hspp_fn  :: FilePath
--       , pi_hspp_buf :: StringBuffer
--       , pi_mod_name_loc :: SrcSpan
--       , pi_mod_name :: ModuleName
--       }

-- -- Preprocess the source file and get its imports
-- -- The pi_local_dflags contains the OPTIONS pragmas
-- getPreprocessedImports
--     :: HscEnv
--     -> FilePath
--     -> Maybe Phase
--     -> Maybe (StringBuffer, UTCTime)
--     -- ^ optional source code buffer and modification time
--     -> ExceptT ErrorMessages IO PreprocessedImports
-- getPreprocessedImports hsc_env src_fn mb_phase maybe_buf = do
--   (pi_local_dflags, pi_hspp_fn)
--       <- ExceptT $ preprocess hsc_env src_fn (fst <$> maybe_buf) mb_phase
--   pi_hspp_buf <- liftIO $ hGetStringBuffer pi_hspp_fn
--   (pi_srcimps, pi_theimps, L pi_mod_name_loc pi_mod_name)
--       <- ExceptT $ do
--           let imp_prelude = xopt LangExt.ImplicitPrelude pi_local_dflags
--               popts = initParserOpts pi_local_dflags
--           mimps <- getImports popts imp_prelude pi_hspp_buf pi_hspp_fn src_fn
--           return (first (fmap pprError) mimps)
--   return PreprocessedImports {..}


-----------------------------------------------------------------------------
--                      Error messages
-----------------------------------------------------------------------------

-- Defer and group warning, error and fatal messages so they will not get lost
-- in the regular output.
withDeferredDiagnostics :: GhcMonad m => m a -> m a
withDeferredDiagnostics f = do
  dflags <- getDynFlags
  if not $ gopt Opt_DeferDiagnostics dflags
  then f
  else do
    warnings <- liftIO $ newIORef []
    errors <- liftIO $ newIORef []
    fatals <- liftIO $ newIORef []
    logger <- getLogger

    let deferDiagnostics _dflags !reason !severity !srcSpan !msg = do
          let action = putLogMsg logger dflags reason severity srcSpan msg
          case severity of
            SevWarning -> atomicModifyIORef' warnings $ \i -> (action: i, ())
            SevError -> atomicModifyIORef' errors $ \i -> (action: i, ())
            SevFatal -> atomicModifyIORef' fatals $ \i -> (action: i, ())
            _ -> action

        printDeferredDiagnostics = liftIO $
          forM_ [warnings, errors, fatals] $ \ref -> do
            -- This IORef can leak when the dflags leaks, so let us always
            -- reset the content.
            actions <- atomicModifyIORef' ref $ \i -> ([], i)
            sequence_ $ reverse actions

    MC.bracket
      (pushLogHookM (const deferDiagnostics))
      (\_ -> popLogHookM >> printDeferredDiagnostics)
      (\_ -> f)

noModError :: HscEnv -> SrcSpan -> ModuleName -> FindResult -> MsgEnvelope DecoratedSDoc
-- ToDo: we don't have a proper line number for this error
noModError hsc_env loc wanted_mod err
  = mkPlainMsgEnvelope loc $ cannotFindModule hsc_env wanted_mod err

noHsFileErr :: SrcSpan -> String -> ErrorMessages
noHsFileErr loc path
  = unitBag $ mkPlainMsgEnvelope loc $ text "Can't find" <+> text path

moduleNotFoundErr :: ModuleName -> ErrorMessages
moduleNotFoundErr mod
  = unitBag $ mkPlainMsgEnvelope noSrcSpan $
        text "module" <+> quotes (ppr mod) <+> text "cannot be found locally"

multiRootsErr :: [ModSummary] -> IO ()
multiRootsErr [] = panic "multiRootsErr"
multiRootsErr summs@(summ1:_)
  = throwOneError $ mkPlainMsgEnvelope noSrcSpan $
        text "module" <+> quotes (ppr mod) <+>
        text "is defined in multiple files:" <+>
        sep (map text files)
  where
    mod = ms_mod summ1
    files = map (expectJust "checkDup" . ml_hs_file . ms_location) summs

keepGoingPruneErr :: [NodeKey] -> SDoc
keepGoingPruneErr ms
  = vcat (( text "-fkeep-going in use, removing the following" <+>
            text "dependencies and continuing:"):
          map (nest 6 . pprNodeKey) ms )

cyclicModuleErr :: [ModuleGraphNode] -> SDoc
-- From a strongly connected component we find
-- a single cycle to report
cyclicModuleErr mss
  = ASSERT( not (null mss) )
    case findCycle graph of
       Nothing   -> text "Unexpected non-cycle" <+> ppr mss
       Just path0 -> vcat
        [ case partitionNodes path0 of
            ([],_) -> text "Module imports form a cycle:"
            (_,[]) -> text "Module instantiations form a cycle:"
            _ -> text "Module imports and instantiations form a cycle:"
        , nest 2 (show_path path0)]
  where
    graph :: [Node NodeKey ModuleGraphNode]
    graph =
      [ DigraphNode
        { node_payload = ms
        , node_key = mkNodeKey ms
        , node_dependencies = get_deps ms
        }
      | ms <- mss
      ]

    get_deps :: ModuleGraphNode -> [NodeKey]
    get_deps = \case
      InstantiationNode iuid ->
        [ NodeKey_Module $ GWIB { gwib_mod = hole, gwib_isBoot = NotBoot }
        | hole <- uniqDSetToList $ instUnitHoles iuid
        ]
      ModuleNode (ExtendedModSummary ms bds) ->
        [ NodeKey_Module $ GWIB { gwib_mod = unLoc m, gwib_isBoot = IsBoot }
        | m <- ms_home_srcimps ms ] ++
        [ NodeKey_Module $ GWIB { gwib_mod = unLoc m, gwib_isBoot = NotBoot }
        | m <- ms_home_imps    ms ] ++
        [ NodeKey_Unit inst_unit
        | inst_unit <- bds
        ]

    show_path :: [ModuleGraphNode] -> SDoc
    show_path []  = panic "show_path"
    show_path [m] = ppr_node m <+> text "imports itself"
    show_path (m1:m2:ms) = vcat ( nest 6 (ppr_node m1)
                                : nest 6 (text "imports" <+> ppr_node m2)
                                : go ms )
       where
         go []     = [text "which imports" <+> ppr_node m1]
         go (m:ms) = (text "which imports" <+> ppr_node m) : go ms

    ppr_node :: ModuleGraphNode -> SDoc
    ppr_node (ModuleNode m) = text "module" <+> ppr_ms (emsModSummary m)
    ppr_node (InstantiationNode u) = text "instantiated unit" <+> ppr u

    ppr_ms :: ModSummary -> SDoc
    ppr_ms ms = quotes (ppr (moduleName (ms_mod ms))) <+>
                (parens (text (msHsFilePath ms)))


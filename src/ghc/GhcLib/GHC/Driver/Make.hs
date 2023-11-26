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

-- This module is partially fetched from GHC's source code repository,
--   as not all useful functions are exported from GHC's library.

--------------------------------------------------------------------------------
module GhcLib.GHC.Driver.Make where

-- module GHC.Driver.Make (
--         depanal, depanalE, depanalPartial,
--         load, load', LoadHowMuch(..),
--         instantiationNodes,

--         downsweep,

--         topSortModuleGraph,

--         ms_home_srcimps, ms_home_imps,

--         summariseModule,
--         hscSourceToIsBoot,
--         findExtraSigImports,
--         implicitRequirementsShallow,

--         noModError, cyclicModuleErr,
--         moduleGraphNodes, SummaryNode,
--         IsBootInterface(..),

--         ModNodeMap(..), emptyModNodeMap, modNodeMapElems, modNodeMapLookup, modNodeMapInsert
--     ) where

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

import qualified Control.Monad.Catch as MC
import Data.IORef
import Data.List (nub, sortBy, partition)
import qualified Data.List as List
import Data.Foldable (toList)
import Data.Maybe
import Data.Ord ( comparing )

import Data.Bifunctor (first)

import System.FilePath
import System.IO        ( fixIO )
import System.IO.Error  ( isDoesNotExistError )

import GHC.Conc ( getNumProcessors, getNumCapabilities, setNumCapabilities )


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
     for a module when we also load object code for all of the imports of the
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
  , UniqSet ModuleName  -- stableBCO -- 
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
        scc :: [ModSummary]
        scc = flattenSCC scc0
        scc_mods :: [ModuleName]
        scc_mods = map ms_mod_name scc

        home_module :: ModuleName -> Bool
        home_module m =
          m `elementOfUniqSet` all_home_mods && m `notElem` scc_mods

        scc_allimps :: [ModuleName]
        scc_allimps = nub (filter home_module (concatMap ms_home_allimps scc))
            -- all imports outside the current SCC, but in the home pkg
        
        stable_obj_imps, stable_bco_imps :: [Bool]
        stable_obj_imps = map (`elementOfUniqSet` stable_obj) scc_allimps
        stable_bco_imps = map (`elementOfUniqSet` stable_bco) scc_allimps

        stableObjects :: Bool
        stableObjects =
           and stable_obj_imps
           && all object_ok scc

        stableBCOs :: Bool
        stableBCOs =
           and (zipWith (||) stable_obj_imps stable_bco_imps)
           && all bco_ok scc

        object_ok :: ModSummary -> Bool
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
                
        bco_ok :: ModSummary -> Bool
        bco_ok ms
          | gopt Opt_ForceRecomp (ms_hspp_opts ms) = False
          | otherwise = case lookupHpt hpt (ms_mod_name ms) of
                Just hmi  | Just l <- hm_linkable hmi ->
                        not (isObjectLinkable l) &&
                        linkableTime l >= ms_hs_date ms
                _other  -> False

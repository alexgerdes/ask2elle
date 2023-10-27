module GhcLib.Utility.Utility(getMaybeSModSummary) where

import GHC qualified
import GHC.Data.EnumSet qualified as GHCEnumSet
import GHC.Driver.Monad qualified as GHC
import GHC.Driver.Session qualified as GHC
import GHC.IO (unsafePerformIO)
import GHC.LanguageExtensions.Type qualified as GHC
import GHC.Plugins qualified as GHC
import GHC.Utils.Logger qualified as GHCLogger

-- | A variant of getModSummary presented in the GHC API documentation.
--  
-- The module may be part of the module graph (see 'hsc_mod_graph' and
-- 'ModuleGraph').  If this is not the case, this function will return Nothing
--
-- This function ignores boot modules and requires that there is only one
-- non-boot module with the given name.
getMaybeSModSummary :: GHC.GhcMonad m => GHC.ModuleName -> m (Maybe GHC.ModSummary)
getMaybeSModSummary moduleName = do
   mg <- GHC.hsc_mod_graph <$> GHC.getSession
   let mods_by_name = [ ms | ms <- GHC.mgModSummaries mg
                      , GHC.ms_mod_name ms == moduleName
                      , GHC.isBootSummary ms == GHC.NotBoot ]
   case mods_by_name of
     [] -> pure Nothing
     [ms] -> pure $ Just ms 
     _ -> pure Nothing
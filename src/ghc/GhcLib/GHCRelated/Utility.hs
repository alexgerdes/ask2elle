module GhcLib.GHCRelated.Utility(getMaybeSModSummary,printCore) where

import GHC qualified
import GHC.Plugins qualified as GHC
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

import GhcLib.GHCRelated.ShowCore 

import System.IO
import Text.Pretty.Simple qualified as PPr

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

-- | The function takes a module name, a core program and a file path and writes the core program to the file path.
printCore :: String -> GHC.CoreProgram -> FilePath -> IO ()
printCore moduleName core path = do 
  withFile path WriteMode $ \handle -> do 
    hPutStrLn handle $ "-- Module: " ++ moduleName
    hPutStrLn handle "-- The following is the direct show instance of core : "
    PPr.pHPrint handle core 
    hPutStrLn handle "-- The following is the pretty printed core: "
    GHC.printSDocLn GHC.defaultSDocContext GHC.ZigZagMode handle (GHC.ppr core)

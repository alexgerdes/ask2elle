{-# LANGUAGE DeriveGeneric #-}
module GhcLib.Compiler.Compiler where


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


import Control.Monad.Catch
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Exception (evaluate)
import Control.Monad.Reader
import Data.Either
import Data.IORef (IORef, newIORef)
import Data.Maybe
import Data.List
import Data.Map
import Data.String (IsString (fromString))
import System.FilePath (takeBaseName)
import System.Process (readProcess)
import System.IO (stdout)

import GhcLib.Utility.Bag ()
import GhcLib.Utility.Flags
import GhcLib.Utility.Utility
import GhcLib.Utility.Warning
import GhcLib.Transform.Transform
import GhcLib.Utility.ShowCore
import Control.Monad.RWS (MonadState(put))
import Debug.Trace (traceM)
import GHC.Generics (Generic)
import GhcLib.Utility.Warning

type ExerciseName = String
type ExercisePath = String


-- data CompInfo = CompInfo
--   { -- compilation information used in feedback generation
--     core :: GHC.CoreProgram
--   , parsed :: GHC.ParsedSource
--   , warns :: [Warning]
--   , names :: Map GHC.Var GHC.Var
--   , exercise :: ExerciseName
--   }
--   deriving (Generic, Show)
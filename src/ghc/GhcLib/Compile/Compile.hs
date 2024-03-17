
module GhcLib.Compile.Compile where



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
import GHC.Utils.Ppr qualified as GHC
import GHC.Core.Opt.Pipeline qualified as GHC
import GHC.Data.StringBuffer qualified as GHC

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
import System.IO (stdout, openFile, IOMode (..), hPutStrLn, hFlush, hClose)

import GhcLib.GHCRelated.Bag ()
import GhcLib.Utility.Flags
import GhcLib.GHCRelated.Utility
import GhcLib.GHCRelated.Warning
import GhcLib.Transform.Transform
import GhcLib.GHCRelated.ShowCore
import GhcLib.Compile.ToCore
import GhcLib.Transform.Inline (recToLetRec)
import GhcLib.Transform.Remove
import GhcLib.Transform.Rename (alpha)
import GhcLib.Transform.Fusion

{- | The type of the function that compiles a program to Core. 
   | It takes an exercise name and a solution, both have the type of String and passed to the ReaderT as configuration.
-}
type CompileFunction = ReaderT ToCoreOption (ExceptT ToCoreError IO) ToCoreOutput 

{- | The entry point for compilation to Core. 
   | This function takes an exercise name and a solution, both have the type of String.AskelleOptions
-}
compileToCore :: String -> String -> CompileFunction -> ExceptT ToCoreError IO ToCoreOutput
compileToCore exerciseName inputSolution f = do
    solution <- liftIO $ GHC.appendStringBuffers (GHC.stringToStringBuffer inputSolution) fusionRule
    runReaderT f $ ToCoreOption solution exerciseName


compSimplNormalised :: ReaderT ToCoreOption (ExceptT ToCoreError IO) ToCoreOutput
-- | Desugar, preprocess and simplify the program, then normalise it
compSimplNormalised = do
    libDirPath' <- liftIO libDirPath
    GHC.defaultErrorHandler
        GHC.defaultFatalMessager
        GHC.defaultFlushOut
        $ GHC.runGhcT (Just libDirPath')
        $ runToCore
        $ do
            (coreProg, parsedSource) <- desugarPreprocessSimplification
            uniqTopLevelLetRecSupply <- liftIO $ GHC.mkSplitUniqSupply 'R'
            fnName <- liftToCore $ asks getStudentExerciseName
            let (normalizedProg, alphaRenamingMapping) = normalise fnName uniqTopLevelLetRecSupply coreProg
            exerciseName <- liftToCore $ asks getStudentExerciseName
            let removedTyEvidenceProg = removeTyEvidence normalizedProg
            return $ ToCoreOutput removedTyEvidenceProg parsedSource alphaRenamingMapping exerciseName

compDesNormalised :: ReaderT ToCoreOption (ExceptT ToCoreError IO) ToCoreOutput
-- | Desugar, preprocess and normalise the program
compDesNormalised = do
    libDirPath' <- liftIO libDirPath
    GHC.defaultErrorHandler
        GHC.defaultFatalMessager
        GHC.defaultFlushOut
        $ GHC.runGhcT (Just libDirPath')
        $ runToCore
        $ do
            (coreProg, parsedSource, alphaRenamingMapping) <- desugarPreprocessNormalize
            exerciseName <- liftToCore $ asks getStudentExerciseName
            return $ ToCoreOutput (removeTyEvidence coreProg) parsedSource alphaRenamingMapping exerciseName


compSimpl :: ReaderT ToCoreOption (ExceptT ToCoreError IO) ToCoreOutput
-- | Desugar, preprocess, simplify the program, alpha renaming
compSimpl = do
    libDirPath' <- liftIO libDirPath
    GHC.defaultErrorHandler
        GHC.defaultFatalMessager
        GHC.defaultFlushOut
        $ GHC.runGhcT (Just libDirPath')
        $ runToCore
        $ do
            (coreProg, parsedSource) <- desugarPreprocessSimplification
            exerciseName <- liftToCore $ asks getStudentExerciseName
            let (coreProg', alphaRenamingMapping) = alpha  exerciseName coreProg
            return $ ToCoreOutput (removeTyEvidence coreProg') parsedSource alphaRenamingMapping exerciseName

compDesugar :: ReaderT ToCoreOption (ExceptT ToCoreError IO) ToCoreOutput
-- | Desugar, preprocess the program, alpha renaming
compDesugar = do
    libDirPath' <- liftIO libDirPath
    GHC.defaultErrorHandler
        GHC.defaultFatalMessager
        GHC.defaultFlushOut
        $ GHC.runGhcT (Just libDirPath')
        $ runToCore
        $ do
            (coreProg, parsedSource) <- desugarPreprocess
            exerciseName <- liftToCore $ asks getStudentExerciseName
            let (coreProg', alphaRenamingMapping) = alpha  exerciseName coreProg
            return $ ToCoreOutput (removeTyEvidence coreProg') parsedSource alphaRenamingMapping exerciseName



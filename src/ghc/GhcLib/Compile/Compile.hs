module GhcLib.Compile.Compile where

import GHC qualified
import GHC.Conc qualified as GHC
import GHC.Core.Opt.Pipeline qualified as GHC
import GHC.Data.Bag qualified as GHC
import GHC.Data.EnumSet qualified as GHCEnumSet
import GHC.Data.Graph.Directed qualified as GHC
import GHC.Data.StringBuffer qualified as GHC
import GHC.Driver.Main qualified as GHC
import GHC.Driver.Make qualified as GHC
import GHC.Driver.Monad qualified as GHC
import GHC.Driver.Session qualified as GHC
import GHC.LanguageExtensions.Type qualified as GHC
import GHC.Linker.Types qualified as GHC
import GHC.Plugins qualified as GHC
import GHC.Runtime.Interpreter qualified as GHC
import GHC.Types.Error qualified as GHC
import GHC.Unit.Home.ModInfo qualified as GHC
import GHC.Unit.Module.Graph qualified as GHC
import GHC.Utils.Error qualified as GHCUtils
import GHC.Utils.Logger qualified as GHCLogger
import GHC.Utils.Ppr qualified as GHC

import Control.Exception (evaluate)
import Control.Monad.Catch
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Reader
import Data.Either
import Data.IORef (IORef, newIORef)
import Data.Map qualified as Map
import Data.Maybe
import Data.String (IsString (fromString))
import System.FilePath (takeBaseName)
import System.IO (IOMode (..), hClose, hFlush, hPutStrLn, openFile, stdout)
import System.Process (readProcess)

import Data.Generics.Biplate (para)
import GhcLib.Compile.ToCore
import GhcLib.GHCRelated.Bag ()
import GhcLib.GHCRelated.ShowCore
import GhcLib.GHCRelated.Utility
import GhcLib.GHCRelated.Warning
import GhcLib.Transform.Fusion
import GhcLib.Transform.Inline (recToLetRec)
import GhcLib.Transform.Remove
import GhcLib.Transform.Rename (alpha)
import GhcLib.Transform.Transform
import GhcLib.Utility.Flags
import Data.List (zip4, zipWith4)

-- | The type of the function that compiles a program to Core.
--    | It takes an exercise name and a solution, both have the type of String and passed to the ReaderT as configuration.
type CompileFunction =
    ReaderT ToCoreInput (ExceptT ToCoreError IO) ToCoreOutput

-- | The entry point for compilation to Core.
--    | This function takes an exercise name and a solution, both have the type of String.AskelleOptions
compileToCore
    :: [(String,String)] -> CompileFunction -> ExceptT ToCoreError IO ToCoreOutput
compileToCore pairOfModuleNameAndSolution compileFun = do
    solutions <- mapM (\(moduleName,solution) -> do
                                        newSolution <- liftIO $
                                            GHC.appendStringBuffers (GHC.stringToStringBuffer solution) fusionRule
                                        pure (moduleName, newSolution)) 
                        pairOfModuleNameAndSolution
    runReaderT compileFun $ ToCoreInput $ map  (uncurry ToCoreProgram) solutions

-- compSimplNormalised :: ReaderT ToCoreInput (ExceptT ToCoreError IO) ToCoreOutput
-- -- | Desugar, preprocess and simplify the program, then normalise it
-- compSimplNormalised = do
--     libDirPath' <- liftIO libDirPath
--     GHC.defaultErrorHandler
--         GHC.defaultFatalMessager
--         GHC.defaultFlushOut
--         $ GHC.runGhcT (Just libDirPath')
--         $ runToCore
--         $ do
--             results <- desugarPreprocessSimplification
--             -- ^ [(coreProg, parsedSource)]
--             uniqTopLevelLetRecSupply <- liftIO $ GHC.mkSplitUniqSupply 'R'
--             fnName <- liftToCore $ asks compilingModuleName
--             let (normalizedProg, alphaRenamingMapping) = normalise fnName uniqTopLevelLetRecSupply coreProg
--             exerciseName <- liftToCore $ asks compilingModuleName
--             let removedTyEvidenceProg = removeTyEvidence normalizedProg
--             return $ ToCoreOutput removedTyEvidenceProg parsedSource alphaRenamingMapping exerciseName


parameterizedCompSimplNormalized
    :: [NormalizationOption]
    -> [PostNormalizationOption]
    -> ReaderT ToCoreInput (ExceptT ToCoreError IO) ToCoreOutput
parameterizedCompSimplNormalized normalizationChoice postNormalizationChoice = do
    libDirPath' <- liftIO libDirPath
    GHC.defaultErrorHandler
        GHC.defaultFatalMessager
        GHC.defaultFlushOut
        $ GHC.runGhcT (Just libDirPath')
        $ runToCore
        $ do
            result <- desugarPreprocessSimplification
            -- ^ [(coreProg, parsedSource)]
            let coreProgs = fmap fst result
            let parsedSource = fmap snd result
            uniqTopLevelLetRecSupply <- liftIO $ GHC.mkSplitUniqSupply 'R'
            -- task <- liftToCore $ asks compilingModuleName
            moduleNames <- fmap compilingModuleName <$> liftToCore (asks getToCoreInput)
            env <- GHC.getSession
            let normalizationOptions' = normalizationOption (head moduleNames) uniqTopLevelLetRecSupply env
            let normalizationOptions = fmap (\moduleName -> normalizationOption moduleName uniqTopLevelLetRecSupply env) moduleNames

            --let normalizedProg = performNormalizationOptions normalizationChoice normalizationOptions' coreProgs
            let normalizeProgs = zipWith (performNormalizationOptions normalizationChoice) normalizationOptions coreProgs
            let postNormalizeProgs = map (performPostNormalizationOptions
                        postNormalizationChoice
                        postNormalizationOption) normalizeProgs

            ---let (alphaedProg, alphaMapping) = alpha task postNormalizedProg
            let alphaResult = zipWith alpha moduleNames postNormalizeProgs
            let alphaedProg = fmap fst alphaResult
            let alphaMapping = fmap snd alphaResult
            return $ ToCoreOutput $ zipWith4 ToCoreOutput' alphaedProg parsedSource alphaMapping moduleNames

-- compDesPreNormalised
--     :: ReaderT ToCoreInput (ExceptT ToCoreError IO) ToCoreOutput

-- -- | Desugar, preprocess and normalise the program
-- compDesPreNormalised = do
--     libDirPath' <- liftIO libDirPath
--     GHC.defaultErrorHandler
--         GHC.defaultFatalMessager
--         GHC.defaultFlushOut
--         $ GHC.runGhcT (Just libDirPath')
--         $ runToCore
--         $ do
--             (coreProg, parsedSource, alphaRenamingMapping) <- desugarPreprocessNormalize
--             exerciseName <- liftToCore $ asks compilingModuleName
--             return $ ToCoreOutput coreProg parsedSource alphaRenamingMapping exerciseName

-- compSimpl :: ReaderT ToCoreInput (ExceptT ToCoreError IO) ToCoreOutput

-- -- | Desugar, preprocess, simplify the program, alpha renaming
-- compSimpl = do
--     libDirPath' <- liftIO libDirPath
--     GHC.defaultErrorHandler
--         GHC.defaultFatalMessager
--         GHC.defaultFlushOut
--         $ GHC.runGhcT (Just libDirPath')
--         $ runToCore
--         $ do
--             (coreProg, parsedSource) <- desugarPreprocessSimplification
--             exerciseName <- liftToCore $ asks compilingModuleName
--             let (coreProg', alphaRenamingMapping) = alpha exerciseName coreProg
--             return $
--                 ToCoreOutput
--                     (removeTyEvidence coreProg')
--                     parsedSource
--                     alphaRenamingMapping
--                     exerciseName

-- compDesugar :: ReaderT ToCoreInput (ExceptT ToCoreError IO) ToCoreOutput

-- -- | Desugar, preprocess the program, alpha renaming
-- compDesugar = do
--     libDirPath' <- liftIO libDirPath
--     GHC.defaultErrorHandler
--         GHC.defaultFatalMessager
--         GHC.defaultFlushOut
--         $ GHC.runGhcT (Just libDirPath')
--         $ runToCore
--         $ do
--             (coreProg, parsedSource) <- desugarPreprocess
--             exerciseName <- liftToCore $ asks compilingModuleName
--             let (coreProg', alphaRenamingMapping) = alpha exerciseName coreProg
--             return $
--                 ToCoreOutput
--                     (removeTyEvidence coreProg')
--                     parsedSource
--                     alphaRenamingMapping
--                     exerciseName

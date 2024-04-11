{-# LANGUAGE StrictData #-}

module GhcLib.Analysis.Analysis
    ( comparePrograms
    , analysisEntryPoint
    , AnalysisInput (..)
    , ComparisonResult (..)
    , SingleComparisonResult (..)
    , ComparisonOutput (..)
    ) where

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
import Data.Char (isLetter)
import Data.Either
import Data.Foldable (foldrM)
import Data.IORef (IORef, newIORef)
import Data.List (isPrefixOf)
import Data.Map qualified as Map
import Data.Maybe
import Data.String (IsString (fromString))
import System.Directory
import System.FilePath
import System.IO
    ( IOMode (..)
    , hClose
    , hFlush
    , hPutStrLn
    , openFile
    , readFile'
    , stdout
    )
import System.Process (readProcess)

import GhcLib.Analysis.SimilarInstance
import GhcLib.Analysis.Utility
import GhcLib.Compile.Compile
import GhcLib.Compile.ToCore
import GhcLib.GHCRelated.Bag ()
import GhcLib.GHCRelated.Utility
import GhcLib.Transform.Inline (recToLetRec)

type StudentSolutionPath = FilePath
type ExerciseName = String

pathToModelSolutions :: FilePath -> FilePath
pathToModelSolutions task = "./ghcTestCases/tasks" </> task </> "modelSolutions"

-- | Not Sure if this is the right way to do it
data ComparisonOutput
    = StudentSolutionInvalid
    | ModelSolutionInvalid
    | Similar
    | NotSimilar
    | NotSimilarButExpected
    | UnexpectedSimilar
    | ExpectedWrong
    deriving (Show, Eq)

comparePrograms
    :: CompileFunction
    -> Bool
    -> (ExerciseName, String)
    -> (ExerciseName, String)
    -> IO ComparisonOutput

-- | Compare two programs, the first boolean input indicates whether the comparison is expected to be similar or not
-- | The first tuple is (the module name of student solution, the student's solution)
-- | The second input is for the model solution
-- | This function prints the core output of both solutions to the current directory
comparePrograms compileFun expectedResult (stdModuleName, studentSolution) (modelModuleName, modelSolution) = do
    stdSolCompOutput <-
        runExceptT $ compileToCore stdModuleName studentSolution compileFun
    modelSolCompOutput <-
        runExceptT $ compileToCore modelModuleName modelSolution compileFun
    case (stdSolCompOutput, modelSolCompOutput) of
        (Left _, _) -> pure StudentSolutionInvalid
        (Right _, Left _) -> pure ModelSolutionInvalid
        (Right (ToCoreOutput stdCore _ _ _), Right (ToCoreOutput modelCore _ _ _)) -> do
            printCore stdModuleName stdCore "./stdCore-output.hs"
            printCore modelModuleName modelCore "./modelCore-output.hs"
            let predecessor = stdCore ~> modelCore
                match = stdCore ~= modelCore
                result = predecessor
            -- \|| match
            -- when (not result && expectedResult) $ putStrLn $ "Failed to match " ++ stdModuleName ++ " with " ++ modelModuleName
            case (result, expectedResult) of
                (True, True) -> pure Similar
                (False, False) -> pure NotSimilarButExpected
                (False, True) -> pure ExpectedWrong
                (True, False) -> pure UnexpectedSimilar

data SingleComparisonResult = SingleComparisonResult
    { modelModuleName :: ExerciseName
    -- ^ The module name of the model solution
    , modelCore :: Maybe GHC.CoreProgram
    -- ^ The core program of the model solution, model solution cannot be invalid
    , comparisonOutput :: ComparisonOutput
    -- ^ The comparison result
    }

data ComparisonResult = ComparisonResult
    { comparisonTaskName :: ExerciseName
    -- ^ The name of the exercise
    , comparedStudentSolution :: String
    -- ^ The content of the student's solution
    , comparedStudentSolutionModuleName :: ExerciseName
    -- ^ The module name of the student's solution
    , comparedStudentSolutionCore :: Either ToCoreError ToCoreOutput
    -- ^ Left indicates that the student solution is invalid, Right contains the core program of the student solution
    , combinedComparisonResults :: [SingleComparisonResult]
    -- ^ Would be empty if the student solution is invalid
    }

data AnalysisInput = AnalysisInput
    { exerciseTarget :: ExerciseName
    -- ^ The name of the exercise
    , studentSolutionModule :: ExerciseName
    -- ^ The module name of the student's solution
    , studentSolutionInput :: String
    -- ^ The content of the student's solution
    , exerciseModelSolution :: [(String, String)]
    -- ^ [(modelSolutionModuleName, modelSolutionContent)]
    }

-- Invariant : The file name,second input, should be the same as the module name, for student's solution, the third input
analysisEntryPoint
    :: ExerciseName -> CompileFunction -> String -> String -> IO ComparisonResult
analysisEntryPoint task f studentModuleName studentSolution = do
    let compileFun = f
    let pathToModelSolutions' = pathToModelSolutions task
    filenameInsideDirectory <- listDirectory pathToModelSolutions'
    modelSolutions <-
        getFilenameAndContent pathToModelSolutions' filenameInsideDirectory
    -- \^ [(moduleName, modelSolutionContent)]
    analyze
        (AnalysisInput task studentModuleName studentSolution modelSolutions)
        compileFun

analyze :: AnalysisInput -> CompileFunction -> IO ComparisonResult
analyze (AnalysisInput task studentModuleName studentSolution modelSolutions) compileFun = do
    stdSolCompOutput <-
        runExceptT $ compileToCore studentModuleName studentSolution compileFun
    -- \^ compile student solution to core
    case stdSolCompOutput of
        Left toCoreErr ->
            pure $
                ComparisonResult task studentSolution studentModuleName (Left toCoreErr) []
        Right stdSolCompileOutpu@(ToCoreOutput stdCore _ _ _) -> do
            comparisonResults <-
                foldrM
                    ( \(modelModuleName, modelSolution) acc -> do
                        compOutput <-
                            compareAgainstModel compileFun True stdCore (modelModuleName, modelSolution)
                        pure $ compOutput : acc
                    )
                    []
                    modelSolutions
            pure $
                ComparisonResult
                    task
                    studentSolution
                    studentModuleName
                    (Right stdSolCompileOutpu)
                    comparisonResults

compareAgainstModel
    :: CompileFunction
    -> Bool
    -> GHC.CoreProgram
    -> (ExerciseName, String)
    -> IO SingleComparisonResult
compareAgainstModel compileFun expectedResult studentCoreProgram (modelModuleName, modelSolution) = do
    modelSolCompOutput <-
        runExceptT $ compileToCore modelModuleName modelSolution compileFun
    case modelSolCompOutput of
        (Left _) -> pure $ SingleComparisonResult modelModuleName Nothing ModelSolutionInvalid
        (Right (ToCoreOutput modelCore _ _ _)) -> do
            let predecessor = studentCoreProgram ~> modelCore
                match = studentCoreProgram ~= modelCore
                result = predecessor || match
            -- when (not result && expectedResult) $ putStrLn $ "Failed to match " ++ stdModuleName ++ " with " ++ modelModuleName
            case (result, expectedResult) of
                (True, True) -> pure $ SingleComparisonResult modelModuleName (Just modelCore) Similar
                (False, False) ->
                    pure $
                        SingleComparisonResult modelModuleName (Just modelCore) NotSimilarButExpected
                (False, True) -> pure $ SingleComparisonResult modelModuleName (Just modelCore) ExpectedWrong
                (True, False) ->
                    pure $
                        SingleComparisonResult modelModuleName (Just modelCore) UnexpectedSimilar

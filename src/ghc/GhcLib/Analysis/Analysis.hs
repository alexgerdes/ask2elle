module GhcLib.Analysis.Analysis(entryPoint) where


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
import System.Process (readProcess)
import System.IO (stdout, openFile, IOMode (..), hPutStrLn, hFlush, hClose, readFile')
import System.Directory
import Data.List (isPrefixOf)
import System.FilePath
import System.IO.Unsafe
import Data.Char (isLetter)

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
import GhcLib.Compile.Compile
import GhcLib.Analysis.SimilarInstance
import Data.Foldable (foldrM)
import GHC (EpAnn(entry))


type StudentSolutionPath = FilePath
type ExerciseName = String


pathToModelSolutions:: FilePath  -> FilePath
pathToModelSolutions task = "./ghcTestCases/tasks" </> task </> "modelSolutions"

pathToStudentSolution :: String
pathToStudentSolution = "./ghcTestCases/tasks/duplicate/shouldMatch/submitSolutions"


data ComparisonOutput =  StudentSolutionInvalid | ModelSolutionInvalid | Similar | NotSimilar | NotSimilarButExpected | UnexpectedSimilar | ExpectedWrong deriving (Show, Eq)

comparePrograms :: CompileFunction -> Bool -> (ExerciseName,String) -> (ExerciseName,String) -> IO ComparisonOutput
comparePrograms compileFun expectedResult (stdModuleName,studentSolution) (modelModuleName,modelSolution) = do
  stdSolCompOutput <- runExceptT $ compileToCore stdModuleName studentSolution compileFun
  modelSolCompOutput <- runExceptT $ compileToCore modelModuleName modelSolution compileFun
  case (stdSolCompOutput,modelSolCompOutput) of
    (Left _,  _) -> pure StudentSolutionInvalid
    (Right _, Left _) -> pure ModelSolutionInvalid
    (Right (ToCoreOutput stdCore _ _ _) , Right (ToCoreOutput modelCore _ _ _)) -> do
      printCore stdModuleName stdCore "./stdCore-output.hs"
      printCore modelModuleName modelCore "./modelCore-output.hs"
      let predecessor = stdCore ~> modelCore
          match = stdCore ~= modelCore
          result = predecessor  -- || match 
      -- when (not result && expectedResult) $ putStrLn $ "Failed to match " ++ stdModuleName ++ " with " ++ modelModuleName
      case (result, expectedResult) of
        (True, True) -> pure Similar
        (False, False) -> pure NotSimilarButExpected
        (False, True) -> pure ExpectedWrong
        (True, False) -> pure  UnexpectedSimilar


test :: IO ()
test = do
  stdSol <- readFile "./ghcTestCases/testfiles/dupli/good/Test3.hs"
  modelSol <- readFile "./ghcTestCases/testfiles/dupli/good/Mod3.hs"
  let stdModuleName = "Test3"
      modelModuleName = "Mod3"
  compOutput <- comparePrograms compSimplNormalised True (stdModuleName,stdSol) (modelModuleName,modelSol)
  print $ show compOutput

data SingleComparisonResult = SingleComparisonResult {
    modelModuleName :: ExerciseName,
    modelCore :: Maybe GHC.CoreProgram,
    comparisonOutput :: ComparisonOutput
    }

data ComparisonResult = ComparisonResult {
    taskName :: ExerciseName, 
    studentSolution :: String,
    studentSolutionInfo :: Maybe ToCoreOutput, -- ^ Nothing indicates that the student solution is invalid
    combinedComparisonResults :: [SingleComparisonResult]
}

entryPoint :: ExerciseName -> String -> IO ()
entryPoint task studentSolution = do
  let compileFun = compSimplNormalised
  comparisonResult <- analyze task studentSolution compileFun
  summarizeComparisonResult comparisonResult

analyze :: ExerciseName -> String -> CompileFunction -> IO ComparisonResult
analyze task studentSolution compileFun = do
  let pathToModelSolutions' = pathToModelSolutions task
  pathToModels <- listDirectory $ pathToModelSolutions task
  -- putStrLn $ pathToModelSolutions task
  -- mapM_ putStrLn pathToModels
  modelSolutions <- foldrM (\path acc -> 
                              if isExtensionOf "hs" path 
                                then do
                                  content <- readFile' $ pathToModelSolutions'  </> path
                                  pure $ (takeBaseName path, content) : acc
                                else pure acc     
                            ) [] pathToModels
  -- mapM_ (putStrLn . show) modelSolutions
  -- ^ [(moduleName, modelSolutionContent)]
  stdSolCompOutput <- runExceptT $ compileToCore task studentSolution compileFun
  -- ^compile student solution to core
  case stdSolCompOutput of
    Left _ -> pure $ ComparisonResult task studentSolution Nothing []
    Right stdSolCompileOutpu@(ToCoreOutput stdCore _ _ _) -> do
      comparisonResults <- foldrM (\(modelModuleName,modelSolution) acc -> do
                              compOutput <- compareAgainstModel compileFun True stdCore (modelModuleName,modelSolution)
                              pure $ compOutput : acc
                              ) [] modelSolutions
      pure $ ComparisonResult task studentSolution (Just stdSolCompileOutpu) comparisonResults

summarizeComparisonResult :: ComparisonResult -> IO ()
summarizeComparisonResult comparisonResult = do 
  putStrLn "Summary of the comparison: "
  let taskName' = taskName comparisonResult
      studentSolution' = studentSolution comparisonResult
      studentSolutionInfo' = studentSolutionInfo comparisonResult
      combinedResults' = combinedComparisonResults comparisonResult
  putStrLn $ "Task: " ++ taskName'
  putStrLn $ "Matched with model solutions: " ++ show (length $ filter (\x -> comparisonOutput x == Similar) combinedResults')
  putStrLn $ "List of model solutions that matched: " ++ show (map modelModuleName $ filter (\x -> comparisonOutput x == Similar) combinedResults')


compareAgainstModel :: CompileFunction -> Bool -> GHC.CoreProgram -> (ExerciseName,String) -> IO SingleComparisonResult
compareAgainstModel compileFun expectedResult studentCoreProgram (modelModuleName,modelSolution) = do
  modelSolCompOutput <- runExceptT $ compileToCore modelModuleName modelSolution compileFun
  case modelSolCompOutput of
    (Left _) -> pure $ SingleComparisonResult modelModuleName Nothing ModelSolutionInvalid
    (Right (ToCoreOutput modelCore _ _ _)) -> do
      let predecessor = studentCoreProgram ~> modelCore
          match = studentCoreProgram ~= modelCore
          result = predecessor || match 
      -- when (not result && expectedResult) $ putStrLn $ "Failed to match " ++ stdModuleName ++ " with " ++ modelModuleName
      case (result, expectedResult) of
        (True, True) -> pure $ SingleComparisonResult modelModuleName (Just modelCore) Similar
        (False, False) -> pure $ SingleComparisonResult modelModuleName (Just modelCore) NotSimilarButExpected
        (False, True) -> pure $ SingleComparisonResult modelModuleName (Just modelCore) ExpectedWrong
        (True, False) -> pure $ SingleComparisonResult modelModuleName (Just modelCore)  UnexpectedSimilar


-- i want to write a testsuite compares the submitted solution with model solutions 
--  what kinda of functionalities do i need?
--   - i need to be able to compile the solution to core
--   - i need to be able to compare the core of the solution with the core of the model solution
--   -   what kind of file system structure suits this purpose?
--   -   i need to track which model solution matches the student solution, which not 
--   -   i need to make summarize based on the result. 
--          
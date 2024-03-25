module GhcLib.Analysis.TestHoleMapping where

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
import Data.Maybe
import Data.Map qualified as Map
import Data.String (IsString (fromString))
import System.Process (readProcess)
import System.IO (stdout, openFile, IOMode (..), hPutStrLn, hFlush, hClose, readFile')
import System.Directory
import Data.List (isPrefixOf)
import System.FilePath
import Data.Char (isLetter)
import Data.Foldable (foldrM)

import GhcLib.Analysis.Analysis
import GhcLib.Analysis.Utility
import GhcLib.GHCRelated.Bag ()
import GhcLib.Utility.Flags
import GhcLib.GHCRelated.Utility
import GhcLib.GHCRelated.Warning
import GhcLib.GHCRelated.ShowCore
import GhcLib.Compile.ToCore
import GhcLib.Compile.Compile
import GhcLib.Analysis.SimilarInstance


analyzeAll :: IO ()
analyzeAll = do
  tasks <- listDirectory "./ghcTestCases/tasks"
  mapM_ analyzeTask tasks

analyzeTask :: String -> IO ()
analyzeTask task = do
  let studentSolutionDirectory = pathToStudentSolution task
  filenamesInsideDirectory <- listDirectory studentSolutionDirectory
  studentSolutions <- getFilenameAndContent studentSolutionDirectory filenamesInsideDirectory
  -- ^ [(moduleName, studentSolutionContent)]
  comparisonResults <- mapM (uncurry $ analysisEntryPoint task) studentSolutions
  -- mapM_ onlyPrintNotSimilar comparisonResult
  let notMatched = filter (not . any
           (\ x -> comparisonOutput x == Similar) . combinedComparisonResults) comparisonResults
  putStrLn "Fail to match any model solution:"
  putStrLn $ "Task: " ++ task
  putStrLn $ "Student solutions : " ++ show (map comparedStudentSolutionModuleName notMatched)
  putStrLn "----------------------------------------------------"
  pure ()




pathToStudentSolution :: String -> FilePath
pathToStudentSolution task = "./ghcTestCases/tasks" </> task </> "shouldMatch/submitSolutions"

summarizeComparisonResult :: ComparisonResult -> IO ()
summarizeComparisonResult comparisonResult = do
  putStrLn "Summary of the comparison: "
  let comparisonTaskName' = comparisonTaskName comparisonResult
      studentSolution' = comparedStudentSolution comparisonResult
      studentSolutionModuleName = comparedStudentSolutionModuleName comparisonResult
      studentSolutionInfo' = comparedStudentSolutionCore comparisonResult
      combinedResults' = combinedComparisonResults comparisonResult
  putStrLn $ "Task: " ++ comparisonTaskName'
  putStrLn $ "Student solution : " ++ studentSolutionModuleName
  putStrLn $ "Matched with model solutions: " ++ show (length $ filter (\x -> comparisonOutput x == Similar) combinedResults')
  putStrLn $ "List of model solutions that matched: " ++ show (map modelModuleName $ filter (\x -> comparisonOutput x == Similar) combinedResults')
  putStrLn "----------------------------------------------------"
  -- unless (any (\x -> comparisonOutput x == Similar) combinedResults') (
  --   writeFile 
  --   )


onlyPrintNotSimilar :: ComparisonResult -> IO ()
onlyPrintNotSimilar comparisonResult
  | predicate comparisonResult = do
      putStrLn "Fail to match any model solution:"
      let comparisonTaskName' = comparisonTaskName comparisonResult
          studentSolution' = comparedStudentSolution comparisonResult
          studentSolutionModuleName = comparedStudentSolutionModuleName comparisonResult
          studentSolutionInfo' = comparedStudentSolutionCore comparisonResult
          combinedResults' = combinedComparisonResults comparisonResult
      putStrLn $ "Task: " ++ comparisonTaskName'
      putStrLn $ "Student solution : " ++ studentSolutionModuleName
      putStrLn "----------------------------------------------------"
  | otherwise = pure ()
  where
    predicate :: ComparisonResult -> Bool
    predicate result  = not (any (\x -> comparisonOutput x == Similar) (combinedComparisonResults result))
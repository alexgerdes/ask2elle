module GhcLib.Analysis.TestHoleMapping where

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
import Data.List (elemIndex, isPrefixOf, partition)
import Data.Map qualified as Map
import Data.Maybe
import Data.String (IsString (fromString))
import GHC.Float (double2Int, int2Double)
import GhcLib.Analysis.Analysis
import GhcLib.Analysis.SimilarInstance
import GhcLib.Analysis.Utility
import GhcLib.Compile.Compile
import GhcLib.Compile.ToCore
import GhcLib.GHCRelated.Bag ()
import GhcLib.GHCRelated.ShowCore
import GhcLib.GHCRelated.Utility
import GhcLib.GHCRelated.Warning
import GhcLib.Transform.Transform
import GhcLib.Utility.Flags
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

analyzeAll :: CompileFunction -> IO ()
analyzeAll f = do
    tasks <- listDirectory "./ghcTestCases/tasks"
    mapM_ (`analyzeTask` f) tasks

data TableColumn = TableColumn
    { normalizationOrder :: [String]
    , postNormalizationOrder :: [String]
    , information :: [String]
    -- ^ comparisonCount, matchedCount, unmatchedCount, ratio
    }

elementIndices :: (Eq a) => [a] -> [a] -> [String]
elementIndices subset full = [index x subset | x <- full]
  where
    index x xs =
        let index = maybe 0 (1 +) (elemIndex x xs)
        in  if 0 == index then "" else show index

calculateRatio
    :: [NormalizationOption] -> [PostNormalizationOption] -> IO TableColumn
calculateRatio normalChoice postNormalChoice = do
    let computationFunction = parameterizedCompSimplNormalized normalChoice postNormalChoice
    tasks <- listDirectory "./ghcTestCases/tasks"
    -- comparisonResult <- concatMap combinedComparisonResults . concat <$>  mapM (`getComparisonResult` f) tasks
    comparisonResult <- mapM (`getComparisonResult` computationFunction) tasks
    let sumbittedStudentSolutionCount = sum $ map length comparisonResult
    let xs =
            fmap
                ( partition
                    ( any
                        (\x -> comparisonOutput x == Similar)
                        . combinedComparisonResults
                    )
                )
                comparisonResult
    -- \^ | every element in the list represents the analysis result for a task,
    --   | the first element is the list of matched student solutions,
    --   | the second element is the list of not matched student solutions
    let matched = sum $ map (length . fst) xs
    let notMatched = sum $ map (length . snd) xs
    pure $
        TableColumn
            (elementIndices normalChoice allNormalizationOptions)
            (elementIndices postNormalChoice allPostNormalizationOptions)
            [ show sumbittedStudentSolutionCount
            , show matched
            , show notMatched
            , show
                ( double2Int $
                    (int2Double matched / int2Double sumbittedStudentSolutionCount) * 100
                )
                ++ "%"
            ]

-- print $ "The number of submitted solution : " ++ show sumbittedStudentSolutionCount
-- let matched = sum $ map (length . fst) xs
-- let notMatched = sum $ map (length . snd) xs
-- print $ "The number of matched solution : " ++ show matched
-- print $ "The number of not matched solution : " ++ show notMatched
-- print $ "The ratio of matched solution : " ++ show (int2Double matched / int2Double sumbittedStudentSolutionCount)

-- | Right now, the return type should be two element
getComparisonResult :: String -> CompileFunction -> IO [ComparisonResult]
getComparisonResult task f = do
    let studentSolutionDirectory = pathToStudentSolution task
    filenamesInsideDirectory <- listDirectory studentSolutionDirectory
    studentSolutions <-
        getFilenameAndContent studentSolutionDirectory filenamesInsideDirectory
    -- \^ [(moduleName, studentSolutionContent)]
    mapM (uncurry $ analysisEntryPoint task f) studentSolutions

-- pure comparisonResults

analyzeTask :: String -> CompileFunction -> IO ()
analyzeTask task f = do
    comparisonResults <- getComparisonResult task f
    -- mapM_ onlyPrintNotSimilar comparisonResult
    let notMatched =
            filter
                ( not
                    . any
                        (\x -> comparisonOutput x == Similar)
                    . combinedComparisonResults
                )
                comparisonResults

    putStrLn "Fail to match any model solution:"
    putStrLn $ "Task: " ++ task
    putStrLn $
        "Student solutions : "
            ++ show (map comparedStudentSolutionModuleName notMatched)
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
    putStrLn $
        "Matched with model solutions: "
            ++ show (length $ filter (\x -> comparisonOutput x == Similar) combinedResults')
    putStrLn $
        "List of model solutions that matched: "
            ++ show
                ( map modelModuleName $
                    filter (\x -> comparisonOutput x == Similar) combinedResults'
                )
    putStrLn "----------------------------------------------------"

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
    predicate result =
        not
            (any (\x -> comparisonOutput x == Similar) (combinedComparisonResults result))

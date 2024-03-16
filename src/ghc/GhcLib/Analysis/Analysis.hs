module GhcLib.Analysis.Analysis(test) where


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
import System.IO (stdout, openFile, IOMode (..), hPutStrLn, hFlush, hClose)
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




type Solution = String
type ExerciseName = String

pathToTestfiles :: String
pathToTestfiles = "./testfiles/"

-- >>> unsafePerformIO $ listDirectory "~/"
-- /tmp/nix-shell.M9ajBh/extra-file-26165019720662-43675-148: withFile: does not exist (No such file or directory)

testAll :: ReaderT ToCoreOption (ExceptT ToCoreError IO) ToCoreOutput -> IO ()
testAll compileFun = do
  exercises <- listDirectory pathToTestfiles
  print exercises
  pure ()
  where
    matchSuffixedFiles :: FilePath -> IO [(FilePath, FilePath)]
    matchSuffixedFiles folderPath = do
      let dir = takeDirectory folderPath
      files <- listDirectory folderPath
      let testFiles = filter (\f -> "Test" `isPrefixOf` f) files
      let modFiles = filter (\f -> "Mod" `isPrefixOf` f) files
      let matchingTuples = [(dir </> f1, dir </> f2) | f1 <- modFiles, f2 <- testFiles, extractSuffix f1 == extractSuffix f2]
      return matchingTuples

    extractSuffix :: FilePath -> String
    extractSuffix haskellFileName = let
      fileNameNoExtension = dropExtension haskellFileName
      in dropWhile isLetter fileNameNoExtension


    succTests :: ExerciseName -> IO [(FilePath, FilePath)]
    succTests ename = matchSuffixedFiles (pathToTestfiles ++ ename ++ "/good/")

    failTests :: ExerciseName -> IO [(FilePath, FilePath)]
    failTests ename = matchSuffixedFiles (pathToTestfiles ++ ename ++ "/bad/")

    normaliseTests :: ExerciseName -> IO [(FilePath, FilePath)]
    normaliseTests ename = matchSuffixedFiles (pathToTestfiles ++ ename ++ "/norm/")

data ComparisonOutput = BothInvalid | StudentSolutionInvalid | ModelSolutionInvalid | Similar | NotSimilar | NotSimilarButExpected | UnexpectedSimilar | ExpectedWrong deriving (Show, Eq)

comparePrograms :: CompileFunction -> Bool -> (ExerciseName,Solution) -> (ExerciseName,Solution) -> IO ComparisonOutput 
comparePrograms compileFun expectedResult (stdModuleName,studentSolution) (modelModuleName,modelSolution) = do 
  stdSolCompOutput <- runExceptT $ compileToCore stdModuleName studentSolution compileFun
  modelSolCompOutput <- runExceptT $ compileToCore modelModuleName modelSolution compileFun
  case (stdSolCompOutput,modelSolCompOutput) of 
    (Left _, Left _) -> return BothInvalid
    (Left _, Right _) -> return StudentSolutionInvalid
    (Right _, Left _) -> return ModelSolutionInvalid
    (Right (ToCoreOutput stdCore _ _ _) , Right (ToCoreOutput modelCore _ _ _)) -> do
      let predecessor = stdCore ~> modelCore 
          match = stdCore ~= modelCore
          result = predecessor || match 
      -- when (not result && expectedResult) $ putStrLn $ "Failed to match " ++ stdModuleName ++ " with " ++ modelModuleName
      case (result, expectedResult) of 
        (True, True) -> return Similar
        (False, False) -> return NotSimilarButExpected
        (False, True) -> pure ExpectedWrong 
        (True, False) -> pure  UnexpectedSimilar

test :: IO () 
test = do 
  stdSol <- readFile "./ghcTestCases/testfiles/dupli/good/Test3.hs"
  modelSol <-readFile "./ghcTestCases/testfiles/dupli/good/Mod3.hs"
  let stdModuleName = "Test3"
      modelModuleName = "Mod3"
  compOutput <- comparePrograms compSimplNormalised True (stdModuleName,stdSol) (modelModuleName,modelSol)
  print $ show compOutput